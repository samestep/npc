use std::{
    env, fmt, fs,
    io::{self, BufRead, Write},
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
use indexmap::IndexMap;
use indicatif::ProgressBar;
use percent_encoding::{NON_ALPHANUMERIC, percent_encode};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Visitor};
use strum::{EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};
use thiserror::Error;

/// The name of this program.
const NAME: &str = env!("CARGO_PKG_NAME");

const GIT: &str = env!("GIT_BIN");
const NIX: &str = env!("NIX_BIN");

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

fn push_newline(mut string: String) -> String {
    string.push('\n');
    string
}

fn pop_newline(mut string: String) -> anyhow::Result<String> {
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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

#[derive(Error, Debug)]
#[error("invalid Nixpkgs branch name")]
struct ParseBranchError;

// This is a hack to get Serde to (de)serialize all the branch names properly.
#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
enum Master {
    Master,
}

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(untagged)]
enum Branch {
    Master(Master),
    Channel(Channel),
}

impl Branch {
    fn master() -> Self {
        Self::Master(Master::Master)
    }
}

impl From<Branch> for &'static str {
    fn from(branch: Branch) -> Self {
        match branch {
            Branch::Master(_) => "master",
            Branch::Channel(channel) => channel.into(),
        }
    }
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad((*self).into())
    }
}

impl FromStr for Branch {
    type Err = ParseBranchError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "master" => Ok(Self::master()),
            _ => match s.parse() {
                Ok(channel) => Ok(Self::Channel(channel)),
                Err(_) => Err(ParseBranchError),
            },
        }
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

enum History {
    Master(Vec<Sha>),
    Channel(IndexMap<Sha, String>),
}

impl History {
    fn new() -> Self {
        Self::Channel(IndexMap::new())
    }

    fn deserialize(json: &str) -> anyhow::Result<Self> {
        Ok(Self::Channel(serde_json::from_str(json)?))
    }

    fn serialize(&self) -> anyhow::Result<String> {
        match self {
            Self::Master(_) => panic!("`master` is a special case"),
            Self::Channel(pairs) => Ok(push_newline(serde_json::to_string_pretty(&pairs)?)),
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Master(shas) => shas.len(),
            Self::Channel(pairs) => pairs.len(),
        }
    }

    fn last(&self) -> Option<Sha> {
        match self {
            Self::Master(_) => panic!("`master` is a special case"),
            Self::Channel(pairs) => pairs.last().map(|(&sha, _)| sha),
        }
    }

    fn get_index(&self, index: usize) -> Option<Sha> {
        match self {
            Self::Master(shas) => shas.get(index).copied(),
            Self::Channel(pairs) => pairs.get_index(index).map(|(&sha, _)| sha),
        }
    }

    /// Be warned that this is linear time.
    fn get_index_of(&self, sha: Sha) -> Option<usize> {
        match self {
            Self::Master(shas) => shas.iter().position(|&rev| rev == sha),
            Self::Channel(pairs) => pairs.get_index_of(&sha),
        }
    }

    fn contains(&self, sha: Sha) -> bool {
        match self {
            Self::Master(_) => panic!("`master` is a special case"),
            Self::Channel(pairs) => pairs.contains_key(&sha),
        }
    }

    fn insert(&mut self, sha: Sha, prefix: String) {
        match self {
            Self::Master(_) => panic!("`master` is a special case"),
            Self::Channel(pairs) => {
                pairs.insert(sha, prefix);
            }
        }
    }
}

impl IntoIterator for History {
    type Item = (Sha, String);

    type IntoIter = <indexmap::IndexMap<Sha, String> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Master(_) => panic!("`master` is a special case"),
            Self::Channel(pairs) => pairs.into_iter(),
        }
    }
}

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
            Ok(datetime) => Some(pop_newline(datetime)?),
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
        Ok(pop_newline(String::from_utf8(output.stdout)?)?.parse()?)
    }

    fn branch(&self, branch: Branch) -> anyhow::Result<History> {
        match branch {
            Branch::Master(_) => {
                let output = self
                    .git()
                    .args(["log", "--first-parent", "--format=%H"])
                    .output()?;
                if !output.status.success() {
                    bail!("failed to list commits from Nixpkgs master branch");
                }
                let mut shas = Vec::new();
                for line in output.stdout.lines() {
                    shas.push(line?.parse()?);
                }
                shas.reverse();
                Ok(History::Master(shas))
            }
            Branch::Channel(channel) => {
                let mut commits = History::deserialize(channel.history()).unwrap();
                let current = History::deserialize(&fs::read_to_string(self.path(channel.key()))?)?;
                for (sha, prefix) in current {
                    commits.insert(sha, prefix);
                }
                Ok(commits)
            }
        }
    }

    fn mark(
        &self,
        path: &Path,
        rev: Option<Sha>,
        mark: impl FnOnce(&mut Bisection, Sha, usize),
    ) -> anyhow::Result<()> {
        let mut bisection = Bisection::new(path)?;
        let rev = match rev {
            // Explicit commit from the CLI takes precedence over current flake input.
            Some(rev) => rev,
            None => match bisection.input {
                None => bail!("please specify a commit"),
                Some(input) => {
                    // The `resolve` function doesn't allow both branch and input to be `Some`.
                    let (_, input) = resolve(None, Some(input))?;
                    // The returned input could only be `None` if we had given a `None` input.
                    let FlakeInput { name, rev } = input.unwrap();
                    bisection.input = Some(name); // Restore the field we took earlier.
                    rev
                }
            },
        };
        let branch = bisection.branch;
        let commits = self.branch(branch)?;
        // As stated in the `get_index_of` docstring, it's not great that this is linear time, but
        // we're only calling it once, so it's probably still better than always hashing everything.
        let Some(index) = commits.get_index_of(rev) else {
            bail!("{rev} not found in history of {branch}");
        };
        mark(&mut bisection, rev, index);
        if let BisectStatus {
            first_bad: Some((sha_bad, index_bad)),
            last_good: Some((sha_good, index_good)),
            done,
        } = bisection.status()
        {
            if index_bad == index_good {
                bail!("{sha_bad} marked both good and bad");
            } else if index_bad < index_good {
                bail!("{sha_bad} marked bad but is earlier than good commit {sha_good}");
            } else if done {
                bisection.next = None;
            } else {
                let Some(sha) = commits.get_index(index_good.midpoint(index_bad)) else {
                    bail!("midpoint commit not found in history of {branch}");
                };
                bisection.next = Some(sha);
            }
        }
        let json = push_newline(serde_json::to_string_pretty(&bisection)?);
        fs::write(path, json)?;
        bisection.print();
        if let Some(input) = &bisection.input {
            let status = bisection.status();
            if status.done
                && let Some((good, _)) = status.last_good
            {
                flake_update(input, good)?;
            } else if let Some(next) = bisection.next {
                flake_update(input, next)?;
            }
        }
        Ok(())
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
            pop_newline(String::from_utf8(output.stdout)?)?
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
        let json = pairs.serialize()?;
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

fn filter_node(node: FlakeNode) -> Option<(Branch, Sha)> {
    if let (
        Some(FlakeOriginal::GitHub {
            owner: owner_original,
            repo: repo_original,
            branch,
        }),
        Some(FlakeLocked::GitHub {
            owner: owner_locked,
            repo: repo_locked,
            rev,
        }),
    ) = (node.original, node.locked)
        && let Ok(branch) = branch.as_deref().unwrap_or("master").parse()
        && owner_original == "NixOS"
        && repo_original == "nixpkgs"
        && owner_locked == "NixOS"
        && repo_locked == "nixpkgs"
    {
        Some((branch, rev))
    } else {
        None
    }
}

fn resolve(
    branch: Option<Branch>,
    flake_input: Option<String>,
) -> anyhow::Result<(Branch, Option<FlakeInput>)> {
    match (branch, flake_input, FlakeLock::new()?) {
        (Some(_), Some(_), _) => bail!("cannot specify both branch and `--input`"),
        (_, Some(_), None) => bail!("specified `--input` but no `flake.lock`"),
        (None, None, None) => bail!("no `flake.lock` found; please specify a branch"),
        (Some(branch), None, _) => Ok((branch, None)),
        (None, Some(name), Some(mut flake_lock)) => {
            let Some(node) = flake_lock.nodes.swap_remove(&name) else {
                bail!("no flake input found named {name}");
            };
            let Some((bran, rev)) = filter_node(node) else {
                bail!("expected Nixpkgs in flake input named {name}");
            };
            Ok((bran, Some(FlakeInput { name, rev })))
        }
        (None, None, Some(flake_lock)) => {
            let mut choices: Vec<_> = flake_lock
                .nodes
                .into_iter()
                .filter_map(|(name, node)| Some((name, filter_node(node)?)))
                .collect();
            let (name, (branch, rev)) = choices
                .pop()
                .ok_or_else(|| anyhow!("no Nixpkgs input found in `flake.lock`"))?;
            if !choices.is_empty() {
                bail!("multiple Nixpkgs inputs in `flake.lock`; please specify `--input`");
            }
            Ok((branch, Some(FlakeInput { name, rev })))
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

struct BisectStatus {
    /// The first known bad commit.
    first_bad: Option<(Sha, usize)>,

    /// The last known good commit.
    last_good: Option<(Sha, usize)>,

    done: bool,
}

#[derive(Deserialize, Serialize)]
struct Bisection {
    branch: Branch,
    input: Option<String>,
    bad: IndexMap<Sha, usize>,
    good: IndexMap<Sha, usize>,
    next: Option<Sha>,
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

    fn status(&self) -> BisectStatus {
        let first_bad = self
            .bad
            .iter()
            .map(|(&sha, &i)| (sha, i))
            .min_by_key(|&(_, i)| i);
        let last_good = self
            .good
            .iter()
            .map(|(&sha, &i)| (sha, i))
            .max_by_key(|&(_, i)| i);
        let mut done = false;
        if let (Some((_, bad)), Some((_, good))) = (first_bad, last_good)
            && bad == good + 1
        {
            done = true;
        }
        BisectStatus {
            first_bad,
            last_good,
            done,
        }
    }

    fn print(&self) {
        let status = self.status();
        if status.done {
            print!("done ");
        }
        print!("bisecting {}", self.branch);
        match &self.input {
            None => println!(),
            Some(input) => println!(" for flake input {input}"),
        }
        match (status.first_bad, status.last_good) {
            (None, None) => println!("waiting for both good and bad commits"),
            (Some(_), None) => println!("waiting for good commit(s), bad commit known"),
            (None, Some(_)) => println!("waiting for bad commit(s), good commit known"),
            (Some((sha_bad, index_bad)), Some((sha_good, index_good))) => {
                if status.done {
                    println!("{sha_bad} is the first bad commit");
                    println!("{sha_good} is the last good commit");
                } else if let Some(next) = self.next
                    && let Some(commits) = index_bad.checked_sub(index_good + 1)
                {
                    let steps = (commits as f64).log2().ceil() as usize;
                    print!("{next} is the next commit; {commits} commit");
                    if commits != 1 {
                        print!("s");
                    }
                    print!(" remain");
                    if commits == 1 {
                        print!("s");
                    }
                    print!(" (roughly {steps} more step");
                    if steps != 1 {
                        print!("s");
                    }
                    println!(")");
                }
            }
        }
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

    /// Print the status of all branches in the cache
    Status,

    /// List commits a branch has pointed to, in reverse chronological order
    Log {
        branch: Option<String>,

        #[clap(long)]
        input: Option<String>,

        #[clap(short = 'n', long)]
        max_count: Option<usize>,
    },

    /// Set a different Nixpkgs commit in `flake.lock`
    Checkout {
        rev: String,

        #[clap(long)]
        input: Option<String>,
    },

    /// Use binary search to find the newest historical commit of a branch before a bug
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
        branch: Option<String>,

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
            let json = push_newline(remote.cache.last_fetched);
            fs::write(path, json)?;
            Ok(())
        }
        (_, Commands::Clean) => unreachable!(),
        (Err(_), _) => {
            bail!("cache missing; please run `{NAME} fetch`");
        }
        (Ok(cache), Commands::Status) => {
            if let Ok(cwd) = env::current_dir()
                && let Ok(bisection) = Bisection::new(&cache.path(CacheKey::Bisect(&cwd)))
            {
                bisection.print();
                println!();
            }

            let width = 21;
            let message1 = "current local time is";
            let message2 = "last fetched cache at";
            assert_eq!(message1.len(), width);
            assert_eq!(message2.len(), width);
            println!("{message1} {}", now());
            println!("{message2} {}", cache.last_fetched);
            println!();
            {
                let output = cache
                    .git()
                    .args(["show", "--no-patch", "--date=iso-local", "--format=%cd"])
                    .output()?;
                if !output.status.success() {
                    bail!("failed to show Git commit date");
                }
                let date = pop_newline(String::from_utf8(output.stdout)?)?;
                println!("{:<width$} {date}", Branch::master());
            }
            for channel in Channel::iter() {
                assert!(<&str>::from(channel).len() <= width);
                match cache.branch(Branch::Channel(channel))?.last() {
                    None => println!("{channel}"),
                    Some(sha) => {
                        let output = cache
                            .git()
                            .args(["show", "--no-patch", "--date=iso-local", "--format=%cd"])
                            .arg(sha.to_string())
                            .output()?;
                        if !output.status.success() {
                            bail!("failed to show Git commit date");
                        }
                        let date = pop_newline(String::from_utf8(output.stdout)?)?;
                        println!("{channel:<width$} {date}");
                    }
                }
            }
            Ok(())
        }
        (
            Ok(cache),
            Commands::Log {
                branch,
                input,
                max_count,
            },
        ) => {
            let branch = branch.map(|name| name.parse()).transpose()?;
            let (branch, _) = resolve(branch, input)?;
            match branch {
                Branch::Master(_) => {
                    let mut cmd = cache.git();
                    cmd.args([
                        "log",
                        "--first-parent",
                        "--date=iso-local",
                        "--format=%H %cd",
                    ]);
                    if let Some(n) = max_count {
                        cmd.arg(format!("--max-count={n}"));
                    }
                    cmd.status()?;
                    Ok(())
                }
                Branch::Channel(_) => {
                    let History::Channel(commits) = cache.branch(branch)? else {
                        unreachable!();
                    };
                    let mut child = cache
                        .git()
                        .args([
                            "log",
                            "--no-walk=unsorted",
                            "--date=iso-local",
                            "--format=%H %cd",
                            "--stdin",
                        ])
                        // We use `--stdin` to avoid possible issues from giving too many arguments.
                        .stdin(Stdio::piped())
                        .spawn()?;
                    // We can't just forward `--max-count` straight through to Git, because that
                    // causes it to ignore the `--no-walk` argument.
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
            }
        }
        (Ok(cache), Commands::Checkout { rev, input }) => {
            let sha = cache.sha(&rev)?;
            let (branch, Some(input)) = resolve(None, input)? else {
                bail!("could not determine flake input to update");
            };
            if let Branch::Channel(_) = branch
                && !cache.branch(branch)?.contains(sha)
            {
                bail!("{sha} not found in history of {branch}");
            };
            flake_update(&input.name, sha)?;
            Ok(())
        }
        (Ok(cache), Commands::Bisect { bisect }) => {
            let path = cache.path(CacheKey::Bisect(&env::current_dir()?));
            match bisect {
                Bisect::Start { branch, input } => {
                    if fs::exists(&path)? {
                        bail!("already bisecting here; to start anew, run `{NAME} bisect reset`");
                    }
                    let branch = branch.map(|name| name.parse()).transpose()?;
                    let (branch, input) = resolve(branch, input)?;
                    let bisection = Bisection {
                        branch,
                        input: input.map(|input| input.name),
                        bad: IndexMap::new(),
                        good: IndexMap::new(),
                        next: None,
                    };
                    let json = push_newline(serde_json::to_string_pretty(&bisection)?);
                    fs::write(path, json)?;
                    bisection.print();
                    Ok(())
                }
                Bisect::Bad { rev } => {
                    let rev = rev.map(|rev| cache.sha(&rev)).transpose()?;
                    cache.mark(&path, rev, |bisection, rev, index| {
                        bisection.bad.insert(rev, index);
                    })?;
                    Ok(())
                }
                Bisect::Good { rev } => {
                    let rev = rev.map(|rev| cache.sha(&rev)).transpose()?;
                    cache.mark(&path, rev, |bisection, rev, index| {
                        bisection.good.insert(rev, index);
                    })?;
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
