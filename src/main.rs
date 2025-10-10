use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
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
use strum::{EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

/// The name of this program.
const NAME: &str = env!("CARGO_PKG_NAME");

const GIT: &str = "git";

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
    chrono::Local::now()
        .format("%Y-%m-%d %H:%M:%S %z")
        .to_string()
}

#[derive(Clone, Copy, EnumIter, EnumString, IntoStaticStr)]
#[strum(serialize_all = "kebab-case")]
enum Channel {
    NixosUnstable,
    NixosUnstableSmall,
    NixpkgsUnstable,
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

    fn key(self) -> CacheKey {
        CacheKey::Channel(self)
    }
}

enum CacheKey {
    LastFetched,
    Git,
    Channel(Channel),
}

impl CacheKey {
    fn name(self) -> String {
        match self {
            Self::LastFetched => "last-fetched.txt".to_owned(),
            Self::Git => "nixpkgs.git".to_owned(),
            Self::Channel(channel) => format!("{}.json", <&str>::from(channel)),
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

        if missing_git || missing_other {
            Ok(Err(PartialCache { dir, missing_git }))
        } else {
            Ok(Ok(Self {
                dir,
                last_fetched: last_fetched.unwrap(),
            }))
        }
    }

    fn git(&self) -> Command {
        let mut cmd = Command::new(GIT);
        cmd.arg("-C").arg(self.dir.join(CacheKey::Git.name()));
        cmd
    }

    fn channel(&self, channel: Channel) -> anyhow::Result<IndexMap<String, String>> {
        Ok(serde_json::from_str(&fs::read_to_string(
            self.dir.join(channel.key().name()),
        )?)?)
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

    async fn git_revision(&self, prefix: &str) -> anyhow::Result<String> {
        let dot = prefix.rfind(".").ok_or_else(|| anyhow!("no dot"))?;
        let short = &prefix[dot + 1..prefix.len() - 1];
        let output = self.cache.git().args(["rev-parse", short]).output()?;
        let sha = if output.status.success() {
            trim_newline(String::from_utf8(output.stdout)?)?
        } else {
            let key = format!("{prefix}git-revision");
            let output = self.s3.get_object().bucket(BUCKET).key(key).send().await?;
            String::from_utf8(output.body.collect().await?.to_vec())?
        };
        Ok(sha)
    }

    async fn list_revisions(
        &self,
        channel: Channel,
        releases: impl IntoIterator<Item = &str>,
        mut callback: impl FnMut(String, String),
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
                    callback(prefix, sha);
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
        let channel_name: &str = channel.into();
        let spinner = ProgressBar::new_spinner();
        spinner.set_message(channel_name);
        spinner.enable_steady_tick(Duration::from_millis(100));
        let mut pairs = IndexMap::new();
        self.list_revisions(channel, releases, |prefix, sha| {
            pairs.insert(prefix, sha);
            spinner.set_message(format!("{channel_name}: {} commits", pairs.len()));
        })
        .await?;
        let mut json = serde_json::to_string_pretty(&pairs)?;
        json.push('\n');
        spinner.finish();
        Ok(json)
    }
}

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
    List { channel: String },

    /// Generate JSON files for unstable channel commits before the most recent release
    History { dir: PathBuf },
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
            // Same format as `--date=iso-local --format=%cd` for Git.
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
                            .arg(cache.dir.join(CacheKey::Git.name()))
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
                fs::write(remote.cache.dir.join(channel.key().name()), json)?;
            }
            // We fetch from Git after fetching from S3 so that, once we're done, all the commit
            // hashes we got from S3 should also be in our local Git clone.
            let status = remote.cache.git().arg("fetch").status()?;
            if !status.success() {
                bail!("failed to fetch from Git");
            }
            let mut last_fetched = remote.cache.last_fetched;
            last_fetched.push('\n');
            fs::write(
                remote.cache.dir.join(CacheKey::LastFetched.name()),
                last_fetched,
            )?;
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
                let name: &str = channel.into();
                assert!(name.len() <= width);
                match cache.channel(channel)?.last() {
                    None => println!("{name}"),
                    Some((_, sha)) => {
                        let output = cache
                            .git()
                            .args([
                                "show",
                                "--no-patch",
                                "--date=iso-local",
                                "--format=%cd",
                                sha,
                            ])
                            .output()?;
                        if !output.status.success() {
                            bail!("failed to show Git commit date");
                        }
                        let date = trim_newline(String::from_utf8(output.stdout)?)?;
                        println!("{name:<width$} {date}");
                    }
                }
            }
            Ok(())
        }
        (Ok(cache), Commands::List { channel }) => {
            let channel = Channel::from_str(&channel)?;
            let history: IndexMap<String, String> =
                serde_json::from_str(channel.history()).unwrap();
            let current = cache.channel(channel)?;
            let mut child = cache
                .git()
                .args([
                    "log",
                    "--no-walk=unsorted",
                    "--date=iso-local",
                    "--format=%H %cd",
                    "--stdin",
                ])
                .stdin(Stdio::piped())
                .spawn()?;
            // We use `--stdin` to avoid possible issues from passing too many arguments.
            for sha in history.values().chain(current.values()).rev() {
                let stdin = child.stdin.as_mut().unwrap();
                stdin.write_all(sha.as_bytes())?;
                stdin.write_all("\n".as_bytes())?;
            }
            child.wait()?;
            Ok(())
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
