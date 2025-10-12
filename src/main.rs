use std::{
    env, fmt, fs,
    io::{self, Write},
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str::FromStr,
    time::Duration,
};

use anyhow::{Context, anyhow, bail};
use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3 as s3;
use clap::{Parser, Subcommand};
use indexmap::{IndexMap, IndexSet};
use indicatif::ProgressBar;
use percent_encoding::{NON_ALPHANUMERIC, percent_encode};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Visitor};
use strum::{EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};
use thiserror::Error;

/// The name of this program.
const NAME: &str = env!("CARGO_PKG_NAME");

const GIT: &str = "git";
const NIX: &str = "nix";

const REMOTES: &str = "
origin\thttps://github.com/NixOS/nixpkgs.git (fetch)
origin\thttps://github.com/NixOS/nixpkgs.git (push)
"
.trim_ascii_start();

const BUCKET: &str = "nix-releases";

// We must specify the same region as the bucket or it responds with a `PermanentRedirect` error.
// This command can be used to check: `curl --head https://nix-releases.s3.amazonaws.com/`
const REGION: Region = Region::from_static("eu-west-1");

// We exclude everything earlier because the bucket does not have `git-revision` objects for those.
const RELEASES: &[&str] = &[
    "16.09", "17.03", "17.09", "18.03", "18.09", "19.03", "19.09", "20.03", "20.09", "21.03",
    "21.05", "21.11", "22.05", "22.11", "23.05", "23.11", "24.05", "24.11", "25.05",
];

const NEXT_RELEASE: &str = "25.11";

fn trim_newline(mut string: String) -> anyhow::Result<String> {
    if string.pop() != Some('\n') {
        bail!("expected trailing newline");
    }
    Ok(string)
}

fn now() -> String {
    // Same format as `--date=iso-local --format=%cd` for Git.
    chrono::Local::now()
        .format("%Y-%m-%d %H:%M:%S %z")
        .to_string()
}

#[derive(Error, Debug)]
#[error("invalid Git SHA")]
struct ParseShaError;

#[derive(Debug, Eq, Hash, PartialEq)]
struct Sha([u8; 20]);

impl fmt::Display for Sha {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for byte in self.0 {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
    }
}

impl FromStr for Sha {
    type Err = ParseShaError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 40 {
            return Err(ParseShaError);
        }
        let mut array = [0u8; 20];
        for (i, chunk) in s.as_bytes().chunks(2).enumerate() {
            let substr = str::from_utf8(chunk).map_err(|_| ParseShaError)?;
            array[i] = u8::from_str_radix(substr, 16).map_err(|_| ParseShaError)?;
        }
        Ok(Sha(array))
    }
}

impl Serialize for Sha {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for Sha {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(ShaVisitor)
    }
}

struct ShaVisitor;

impl<'de> Visitor<'de> for ShaVisitor {
    type Value = Sha;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a Git SHA")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        v.parse().map_err(E::custom)
    }
}

#[derive(
    Clone, Copy, Deserialize, EnumIter, EnumString, Eq, IntoStaticStr, PartialEq, Serialize,
)]
#[serde(rename_all = "kebab-case")]
#[strum(serialize_all = "kebab-case")]
enum Channel {
    NixosUnstable,
    NixosUnstableSmall,
    NixpkgsUnstable,
}

impl fmt::Display for Channel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.into())
    }
}

impl Channel {
    fn history(self) -> &'static str {
        match self {
            Self::NixosUnstable => include_str!("nixos-unstable.json"),
            Self::NixosUnstableSmall => include_str!("nixos-unstable-small.json"),
            Self::NixpkgsUnstable => include_str!("nixpkgs-unstable.json"),
        }
    }

    fn prefix(self, release: &str) -> String {
        let prefix = match self {
            Self::NixosUnstable => "nixos/unstable/nixos",
            Self::NixosUnstableSmall => "nixos/unstable-small/nixos",
            Self::NixpkgsUnstable => "nixpkgs/nixpkgs",
        };
        format!("{prefix}-{release}pre")
    }

    fn key(self) -> CacheKey<'static> {
        CacheKey::Channel(self)
    }
}

enum CacheKey<'a> {
    LastFetched,
    Git,
    Channel(Channel),
    Bisect(&'a Path),
}

impl CacheKey<'_> {
    fn name(self) -> String {
        match self {
            Self::LastFetched => "last-fetched.txt".to_owned(),
            Self::Git => "nixpkgs.git".to_owned(),
            Self::Channel(channel) => format!("{channel}.json"),
            Self::Bisect(path) => {
                assert!(path.is_absolute());
                // We use percent encoding to turn an entire path into a single pathname component.
                let encoded = percent_encode(path.as_os_str().as_bytes(), NON_ALPHANUMERIC);
                format!("bisect/{encoded}.json")
            }
        }
    }
}

type History = IndexMap<Sha, String>;

struct PartialCache {
    dir: PathBuf,
    missing_git: bool,
}

struct Cache {
    dir: PathBuf,
    last_fetched: String,
}

impl Cache {
    fn new(dir: PathBuf) -> anyhow::Result<Result<Self, PartialCache>> {
        let mut missing_git = false;
        let mut missing_other = false;

        let last_fetched = match fs::read_to_string(dir.join(CacheKey::LastFetched.name())) {
            Ok(datetime) => Some(trim_newline(datetime)?),
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => None,
                _ => bail!(err),
            },
        };
        if last_fetched.is_none() {
            missing_other = true;
        }

        let git_dir = dir.join(CacheKey::Git.name());
        if fs::exists(&git_dir)? {
            let output = Command::new(GIT)
                .arg("-C")
                .arg(&git_dir)
                .args(["remote", "--verbose"])
                .stderr(Stdio::inherit())
                .output()?;
            if !output.status.success() {
                bail!("failed to list Git remotes in {}", git_dir.display());
            }
            if str::from_utf8(&output.stdout)? != REMOTES {
                bail!("unexpected Git remotes in {}", git_dir.display())
            }
        } else {
            missing_git = true;
        }

        for channel in Channel::iter() {
            let key = channel.key();
            if !fs::exists(dir.join(key.name()))? {
                missing_other = true;
            }
        }

        fs::create_dir_all(dir.join("bisect"))?;

        if missing_git || missing_other {
            Ok(Err(PartialCache { dir, missing_git }))
        } else {
            Ok(Ok(Self {
                dir,
                last_fetched: last_fetched.unwrap(),
            }))
        }
    }

    fn path(&self, key: CacheKey) -> PathBuf {
        self.dir.join(key.name())
    }

    fn git(&self) -> Command {
        let mut cmd = Command::new(GIT);
        cmd.arg("-C").arg(self.path(CacheKey::Git));
        cmd
    }

    fn sha(&self, rev: &str) -> anyhow::Result<Sha> {
        let output = self
            .git()
            .args(["rev-parse", rev])
            .stderr(Stdio::inherit())
            .output()?;
        if !output.status.success() {
            bail!("failed to disambiguate Nixpkgs commit {rev}");
        }
        Ok(trim_newline(String::from_utf8(output.stdout)?)?.parse()?)
    }

    fn channel(&self, channel: Channel) -> anyhow::Result<History> {
        let mut commits: History = serde_json::from_str(channel.history()).unwrap();
        let current: History =
            serde_json::from_str(&fs::read_to_string(self.path(channel.key()))?)?;
        for (sha, prefix) in current {
            commits.insert(sha, prefix);
        }
        Ok(commits)
    }
}

struct Remote {
    cache: Cache,
    s3: s3::Client,
}

impl Remote {
    async fn new(cache: Cache) -> Self {
        // We use `no_credentials` to avoid authenticating because it's a public bucket.
        let config = aws_config::defaults(BehaviorVersion::latest())
            .no_credentials()
            .region(REGION)
            .load()
            .await;
        let s3 = s3::Client::new(&config);
        Self { cache, s3 }
    }

    async fn git_revision(&self, prefix: &str) -> anyhow::Result<Sha> {
        let dot = prefix.rfind(".").ok_or_else(|| anyhow!("no dot"))?;
        let short = &prefix[dot + 1..prefix.len() - 1];
        let output = self.cache.git().args(["rev-parse", short]).output()?;
        let string = if output.status.success() {
            trim_newline(String::from_utf8(output.stdout)?)?
        } else {
            let key = format!("{prefix}git-revision");
            let output = self.s3.get_object().bucket(BUCKET).key(key).send().await?;
            String::from_utf8(output.body.collect().await?.to_vec())?
        };
        Ok(string.parse()?)
    }

    async fn list_revisions(
        &self,
        channel: Channel,
        releases: impl IntoIterator<Item = &str>,
        mut callback: impl FnMut(Sha, String),
    ) -> anyhow::Result<()> {
        for release in releases {
            let mut continuation_token = None;
            loop {
                let output = self
                    .s3
                    .list_objects_v2()
                    .bucket(BUCKET)
                    .prefix(channel.prefix(release))
                    .delimiter("/")
                    .set_continuation_token(continuation_token)
                    .send()
                    .await?;
                for item in output.common_prefixes.unwrap_or_default() {
                    let prefix = item.prefix.ok_or_else(|| anyhow!("missing prefix"))?;
                    let sha = self.git_revision(&prefix).await?;
                    callback(sha, prefix);
                }
                match output.next_continuation_token {
                    Some(token) => continuation_token = Some(token),
                    None => break,
                };
            }
        }
        Ok(())
    }

    async fn channel_json(
        &self,
        channel: Channel,
        releases: impl IntoIterator<Item = &str>,
    ) -> anyhow::Result<String> {
        let spinner = ProgressBar::new_spinner();
        spinner.set_message(<&str>::from(channel));
        spinner.enable_steady_tick(Duration::from_millis(100));
        let mut pairs = History::new();
        self.list_revisions(channel, releases, |sha, prefix| {
            pairs.insert(sha, prefix);
            spinner.set_message(format!("{channel}: {} commits", pairs.len()));
        })
        .await?;
        let mut json = serde_json::to_string_pretty(&pairs)?;
        json.push('\n');
        spinner.finish();
        Ok(json)
    }
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
enum FlakeOriginal {
    #[serde(rename = "github")]
    GitHub {
        owner: String,

        repo: String,

        #[serde(rename = "ref")]
        branch: Option<String>,
    },

    #[serde(other)]
    Other,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
enum FlakeLocked {
    #[serde(rename = "github")]
    GitHub {
        owner: String,
        repo: String,
        rev: Sha,
    },

    #[serde(other)]
    Other,
}

#[derive(Debug, Deserialize)]
struct FlakeNode {
    original: Option<FlakeOriginal>,
    locked: Option<FlakeLocked>,
}

#[derive(Debug, Deserialize)]
struct FlakeLock {
    nodes: IndexMap<String, FlakeNode>,
}

impl FlakeLock {
    fn new() -> anyhow::Result<Option<Self>> {
        let err = match fs::read_to_string("flake.lock") {
            Ok(json) => match serde_json::from_str(&json) {
                Ok(flake_lock) => return Ok(Some(flake_lock)),
                Err(err) => anyhow!(err),
            },
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => return Ok(None),
                _ => anyhow!(err),
            },
        };
        Err(err.context("`flake.lock` broken"))
    }
}

struct FlakeInput {
    name: String,
    rev: Sha,
}

fn filter_node(node: FlakeNode) -> Option<(Channel, Sha)> {
    if let (
        Some(FlakeOriginal::GitHub {
            owner: owner_original,
            repo: repo_original,
            branch: Some(branch),
        }),
        Some(FlakeLocked::GitHub {
            owner: owner_locked,
            repo: repo_locked,
            rev,
        }),
    ) = (node.original, node.locked)
        && let Ok(channel) = Channel::from_str(&branch)
        && owner_original == "NixOS"
        && repo_original == "nixpkgs"
        && owner_locked == "NixOS"
        && repo_locked == "nixpkgs"
    {
        Some((channel, rev))
    } else {
        None
    }
}

fn resolve(
    channel: Option<Channel>,
    flake_input: Option<String>,
) -> anyhow::Result<(Channel, Option<FlakeInput>)> {
    match (channel, flake_input, FlakeLock::new()?) {
        (Some(_), Some(_), _) => bail!("cannot specify both channel and `--input`"),
        (_, Some(_), None) => bail!("specified `--input` but no `flake.lock`"),
        (None, None, None) => bail!("no `flake.lock` found; please specify a channel"),
        (Some(channel), None, _) => Ok((channel, None)),
        (None, Some(name), Some(mut flake_lock)) => {
            let Some(node) = flake_lock.nodes.swap_remove(&name) else {
                bail!("no flake input found named {name}");
            };
            let Some((chan, rev)) = filter_node(node) else {
                bail!("expected Nixpkgs in flake input named {name}");
            };
            Ok((chan, Some(FlakeInput { name, rev })))
        }
        (None, None, Some(flake_lock)) => {
            let mut choices: Vec<_> = flake_lock
                .nodes
                .into_iter()
                .filter_map(|(name, node)| Some((name, filter_node(node)?)))
                .collect();
            let (name, (channel, rev)) = choices
                .pop()
                .ok_or_else(|| anyhow!("no Nixpkgs input found in `flake.lock`"))?;
            if !choices.is_empty() {
                bail!("multiple Nixpkgs inputs in `flake.lock`; please specify `--input`");
            }
            Ok((channel, Some(FlakeInput { name, rev })))
        }
    }
}

fn flake_update(name: &str, sha: Sha) -> anyhow::Result<()> {
    let status = Command::new(NIX)
        .args([
            "flake",
            "update",
            name,
            "--override-input",
            name,
            &format!("github:NixOS/nixpkgs/{sha}"),
        ])
        .status()?;
    if !status.success() {
        bail!("failed to update `flake.lock`");
    }
    Ok(())
}

#[derive(Deserialize, Serialize)]
struct Bisection {
    channel: Channel,
    input: Option<String>,
    bad: IndexSet<Sha>,
    good: IndexSet<Sha>,
}

impl Bisection {
    fn new(path: &Path) -> anyhow::Result<Self> {
        let err = match fs::read_to_string(path) {
            Ok(json) => match serde_json::from_str(&json) {
                Ok(info) => return Ok(info),
                Err(err) => anyhow!(err),
            },
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => {
                    bail!("not currently bisecting here; please run `{NAME} bisect start`");
                }
                _ => anyhow!(err),
            },
        };
        Err(err.context(format!(
            "bisection state broken; please run `{NAME} bisect reset`"
        )))
    }
}

/// Nixpkgs channel history CLI
#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Update local cache and Git clone of Nixpkgs
    Fetch,

    /// Delete the cache
    Clean,

    /// Print the status of all channels in the cache
    Status,

    /// List commits for an unstable channel in reverse chronological order
    List {
        channel: Option<String>,

        #[clap(long)]
        input: Option<String>,

        #[clap(short = 'n', long)]
        max_count: Option<usize>,
    },

    /// Set a different Nixpkgs commit in `flake.lock`
    Checkout {
        rev: String,

        channel: Option<String>,

        #[clap(long)]
        input: Option<String>,
    },

    /// Use binary search to find the newest historical commit of a channel before a bug
    Bisect {
        #[command(subcommand)]
        bisect: Bisect,
    },

    /// Generate JSON files for unstable channel commits before the most recent release
    History { dir: PathBuf },
}

#[derive(Subcommand)]
enum Bisect {
    /// Start bisecting in this directory
    Start {
        channel: Option<String>,

        #[clap(long)]
        input: Option<String>,
    },

    /// Mark this commit as "bad"
    #[command(visible_alias = "new")]
    Bad { rev: Option<String> },

    /// Mark this commit as "good"
    #[command(visible_alias = "old")]
    Good { rev: Option<String> },

    /// Delete bisection state for this directory
    Reset,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    // We parse CLI args before checking the cache, to allow early exit for `--help`.
    let cache_dir = dirs::cache_dir()
        .ok_or_else(|| anyhow!("no cache directory"))?
        .join(format!("nix-{NAME}"));
    if let Commands::Clean = args.command {
        fs::remove_dir_all(cache_dir)?;
        return Ok(());
    }
    // We checked for the `clean` subcommand first since it must still work if the cache is broken.
    match (
        Cache::new(cache_dir)
            .with_context(|| format!("cache broken; please run `{NAME} clean`"))?,
        args.command,
    ) {
        (cache_result, Commands::Fetch) => {
            let last_fetched = now();
            let cache = match cache_result {
                Ok(mut cache) => {
                    cache.last_fetched = last_fetched;
                    cache
                }
                Err(PartialCache { dir, missing_git }) => {
                    let cache = Cache { dir, last_fetched };
                    if missing_git {
                        let repo = "https://github.com/NixOS/nixpkgs.git";
                        let status = Command::new(GIT)
                            .args(["clone", "--mirror", repo])
                            .arg(cache.path(CacheKey::Git))
                            .status()?;
                        if !status.success() {
                            bail!("failed to clone {repo}");
                        }
                    }
                    cache
                }
            };
            let remote = Remote::new(cache).await;
            for channel in Channel::iter() {
                let json = remote.channel_json(channel, [NEXT_RELEASE]).await?;
                fs::write(remote.cache.path(channel.key()), json)?;
            }
            // We fetch from Git after fetching from S3 so that, once we're done, all the commit
            // hashes we got from S3 should also be in our local Git clone.
            let status = remote
                .cache
                .git()
                .args(["fetch", "--no-show-forced-updates"])
                .status()?;
            // Without the `--no-show-forced-updates` flag, Git prints spends a lot of time figuring
            // out that all the updates to refs/pull/*/head and refs/pull/*/merge were forced.
            if !status.success() {
                bail!("failed to fetch from Git");
            }
            let path = remote.cache.path(CacheKey::LastFetched);
            let mut last_fetched = remote.cache.last_fetched;
            last_fetched.push('\n');
            fs::write(path, last_fetched)?;
            Ok(())
        }
        (_, Commands::Clean) => unreachable!(),
        (Err(_), _) => {
            bail!("cache missing; please run `{NAME} fetch`");
        }
        (Ok(cache), Commands::Status) => {
            let width = 21;
            let message1 = "current local time is";
            let message2 = "last fetched cache at";
            assert_eq!(message1.len(), width);
            assert_eq!(message2.len(), width);
            println!("{message1} {}", now());
            println!("{message2} {}", cache.last_fetched);
            println!();
            for channel in Channel::iter() {
                assert!(<&str>::from(channel).len() <= width);
                match cache.channel(channel)?.last() {
                    None => println!("{channel}"),
                    Some((sha, _)) => {
                        let output = cache
                            .git()
                            .args(["show", "--no-patch", "--date=iso-local", "--format=%cd"])
                            .arg(sha.to_string())
                            .output()?;
                        if !output.status.success() {
                            bail!("failed to show Git commit date");
                        }
                        let date = trim_newline(String::from_utf8(output.stdout)?)?;
                        println!("{channel:<width$} {date}");
                    }
                }
            }
            Ok(())
        }
        (
            Ok(cache),
            Commands::List {
                channel,
                input,
                max_count,
            },
        ) => {
            let channel = channel.map(|name| Channel::from_str(&name)).transpose()?;
            let (channel, _) = resolve(channel, input)?;
            let commits = cache.channel(channel)?;
            let mut child = cache
                .git()
                .args([
                    "log",
                    "--no-walk=unsorted",
                    "--date=iso-local",
                    "--format=%H %cd",
                    "--stdin",
                ])
                // We use `--stdin` to avoid possible issues from passing too many arguments.
                .stdin(Stdio::piped())
                .spawn()?;
            // We can't just forward `--max-count` straight through to Git, because that causes it
            // to ignore the `--no-walk` argument.
            for sha in commits
                .keys()
                .rev()
                .take(max_count.unwrap_or(commits.len()))
            {
                let stdin = child.stdin.as_mut().unwrap();
                stdin.write_all(sha.to_string().as_bytes())?;
                stdin.write_all("\n".as_bytes())?;
            }
            child.wait()?;
            Ok(())
        }
        (
            Ok(cache),
            Commands::Checkout {
                rev,
                channel,
                input,
            },
        ) => {
            let sha = cache.sha(&rev)?;
            let channel = channel.map(|name| Channel::from_str(&name)).transpose()?;
            let (channel, Some(input)) = resolve(channel, input)? else {
                bail!("could not determine flake input to update");
            };
            if !cache.channel(channel)?.contains_key(&sha) {
                bail!("the history of {channel} does not contain commit {sha}");
            };
            flake_update(&input.name, sha)?;
            Ok(())
        }
        (Ok(cache), Commands::Bisect { bisect }) => {
            let path = cache.path(CacheKey::Bisect(&env::current_dir()?));
            match bisect {
                Bisect::Start { channel, input } => {
                    if fs::exists(&path)? {
                        bail!("already bisecting here; to start anew, run `{NAME} bisect reset`");
                    }
                    let channel = channel.map(|name| Channel::from_str(&name)).transpose()?;
                    let (channel, input) = resolve(channel, input)?;
                    let bisection = Bisection {
                        channel,
                        input: input.map(|input| input.name),
                        bad: IndexSet::new(),
                        good: IndexSet::new(),
                    };
                    let mut json = serde_json::to_string_pretty(&bisection)?;
                    json.push('\n');
                    fs::write(path, json)?;
                    Ok(())
                }
                Bisect::Bad { rev } => {
                    let rev = rev.map(|rev| cache.sha(&rev)).transpose()?;
                    let mut bisection = Bisection::new(&path)?;
                    let (_, input) = resolve(Some(bisection.channel), bisection.input.clone())?;
                    let rev = match (rev, input) {
                        (Some(rev), _) => rev,
                        // Explicit commit from the CLI takes precedence over current flake input.
                        (None, Some(input)) => input.rev,
                        (None, None) => bail!("please specify a commit"),
                    };
                    bisection.bad.insert(rev);
                    let mut json = serde_json::to_string_pretty(&bisection)?;
                    json.push('\n');
                    fs::write(path, json)?;
                    // TODO: Update `flake.lock` with a different Nixpkgs commit.
                    Ok(())
                }
                Bisect::Good { rev } => {
                    let rev = rev.map(|rev| cache.sha(&rev)).transpose()?;
                    let mut bisection = Bisection::new(&path)?;
                    let (_, input) = resolve(Some(bisection.channel), bisection.input.clone())?;
                    let rev = match (rev, input) {
                        (Some(rev), _) => rev,
                        // Explicit commit from the CLI takes precedence over current flake input.
                        (None, Some(input)) => input.rev,
                        (None, None) => bail!("please specify a commit"),
                    };
                    bisection.good.insert(rev);
                    let mut json = serde_json::to_string_pretty(&bisection)?;
                    json.push('\n');
                    fs::write(path, json)?;
                    // TODO: Update `flake.lock` with a different Nixpkgs commit.
                    Ok(())
                }
                Bisect::Reset => fs::remove_file(path).or_else(|err| match err.kind() {
                    io::ErrorKind::NotFound => Ok(()),
                    _ => Err(anyhow!(err)),
                }),
            }
        }
        (Ok(cache), Commands::History { dir }) => {
            let remote = Remote::new(cache).await;
            for channel in Channel::iter() {
                let json = remote
                    .channel_json(channel, RELEASES.iter().copied())
                    .await?;
                fs::write(dir.join(channel.key().name()), json)?;
            }
            Ok(())
        }
    }
}
