use std::{
    fs,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
    time::Duration,
};

use anyhow::{anyhow, bail};
use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3 as s3;
use clap::{Parser, Subcommand};
use indexmap::IndexMap;
use indicatif::ProgressBar;

/// The name of this program.
const NAME: &str = "nixpkgs";

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

const UNSTABLE_CHANNELS: &[&str] = &["nixos-unstable", "nixos-unstable-small", "nixpkgs-unstable"];

// We exclude everything earlier because the bucket does not have `git-revision` objects for those.
const RELEASES: &[&str] = &[
    "16.09", "17.03", "17.09", "18.03", "18.09", "19.03", "19.09", "20.03", "20.09", "21.03",
    "21.05", "21.11", "22.05", "22.11", "23.05", "23.11", "24.05", "24.11", "25.05",
];

const NEXT_RELEASE: &str = "25.11";

fn unstable_channel_history(channel: &str) -> Option<&'static str> {
    match channel {
        "nixos-unstable" => Some(include_str!("nixos-unstable.json")),
        "nixos-unstable-small" => Some(include_str!("nixos-unstable-small.json")),
        "nixpkgs-unstable" => Some(include_str!("nixpkgs-unstable.json")),
        _ => None,
    }
}

fn unstable_channel_prefix(channel: &str, release: &str) -> String {
    let prefix = match channel {
        "nixos-unstable" => "nixos/unstable/nixos",
        "nixos-unstable-small" => "nixos/unstable-small/nixos",
        "nixpkgs-unstable" => "nixpkgs/nixpkgs",
        _ => unimplemented!("{channel}"),
    };
    format!("{prefix}-{release}pre")
}

fn cache_dir() -> anyhow::Result<PathBuf> {
    Ok(dirs::cache_dir()
        .ok_or_else(|| anyhow!("no cache directory"))?
        .join(NAME))
}

fn cached(channel: &str) -> IndexMap<String, String> {
    let Ok(dir) = cache_dir() else {
        return Default::default();
    };
    let Ok(json) = fs::read_to_string(dir.join(format!("{channel}.json"))) else {
        return Default::default();
    };
    serde_json::from_str(&json).unwrap_or_default()
}

struct Clone {
    dir: PathBuf,
}

impl Clone {
    fn new() -> anyhow::Result<Self> {
        let repo = "https://github.com/NixOS/nixpkgs.git";
        let dir = cache_dir()?.join("nixpkgs.git");
        if fs::exists(&dir)? {
            let output = Command::new(GIT)
                .arg("-C")
                .arg(&dir)
                .args(["remote", "--verbose"])
                .stderr(Stdio::inherit())
                .output()?;
            if !output.status.success() {
                bail!("failed to list Git remotes in {}", dir.display());
            }
            if str::from_utf8(&output.stdout)? != REMOTES {
                bail!("unexpected Git remotes in {}", dir.display())
            }
        } else {
            eprintln!("Cloning {repo} which will use several gigabytes of disk space.");
            let status = Command::new(GIT)
                .args(["clone", "--mirror", repo])
                .arg(&dir)
                .status()?;
            if !status.success() {
                bail!("failed to clone {repo}");
            }
        }
        Ok(Self { dir })
    }

    fn git(&self) -> Command {
        let mut cmd = Command::new(GIT);
        cmd.arg("-C").arg(&self.dir);
        cmd
    }
}

struct Remote {
    clone: Clone,
    s3: s3::Client,
}

impl Remote {
    async fn new(clone: Clone) -> Self {
        // We use `no_credentials` to avoid authenticating because it's a public bucket.
        let config = aws_config::defaults(BehaviorVersion::latest())
            .no_credentials()
            .region(REGION)
            .load()
            .await;
        let s3 = s3::Client::new(&config);
        Self { clone, s3 }
    }

    async fn git_revision(&self, prefix: &str) -> anyhow::Result<String> {
        let dot = prefix.rfind(".").ok_or_else(|| anyhow!("no dot"))?;
        let short = &prefix[dot + 1..prefix.len() - 1];
        let output = self.clone.git().args(["rev-parse", short]).output()?;
        if output.status.success() {
            let mut sha = String::from_utf8(output.stdout)?;
            if sha.pop() != Some('\n') {
                bail!("expected trailing newline from `git rev-parse`");
            }
            Ok(sha)
        } else {
            let key = format!("{prefix}git-revision");
            let output = self.s3.get_object().bucket(BUCKET).key(key).send().await?;
            let sha = String::from_utf8(output.body.collect().await?.to_vec())?;
            Ok(sha)
        }
    }

    async fn list_revisions(
        &self,
        channel: &str,
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
                    .prefix(unstable_channel_prefix(channel, release))
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
        channel: &'static str,
        releases: impl IntoIterator<Item = &str>,
    ) -> anyhow::Result<String> {
        let spinner = ProgressBar::new_spinner();
        spinner.set_message(channel);
        spinner.enable_steady_tick(Duration::from_millis(100));
        let mut pairs = IndexMap::new();
        self.list_revisions(channel, releases, |prefix, sha| {
            pairs.insert(prefix, sha);
            spinner.set_message(format!("{channel}: {} commits", pairs.len()));
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

    /// List commits for an unstable channel in reverse chronological order
    List { channel: String },

    /// Generate JSON files for unstable channel commits before the most recent release
    History { dir: PathBuf },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    // We parse CLI args before checking the Git clone to allow early exit for `--help`.
    let clone = Clone::new()?;
    match args.command {
        Commands::Fetch => {
            let remote = Remote::new(clone).await;
            let dir = cache_dir()?;
            for channel in UNSTABLE_CHANNELS {
                let json = remote.channel_json(channel, [NEXT_RELEASE]).await?;
                fs::write(dir.join(format!("{channel}.json")), json)?;
            }
            // We fetch from Git after fetching from S3 so that, once we're done, all the commit
            // hashes we got from S3 should also be in our local Git clone.
            let status = remote.clone.git().arg("fetch").status()?;
            if !status.success() {
                bail!("failed to fetch from Git");
            }
            Ok(())
        }
        Commands::List { channel } => {
            let history: IndexMap<String, String> =
                serde_json::from_str(unstable_channel_history(&channel).unwrap()).unwrap();
            let current = cached(&channel);
            let mut child = clone
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
        Commands::History { dir } => {
            let remote = Remote::new(clone).await;
            for channel in UNSTABLE_CHANNELS {
                let json = remote
                    .channel_json(channel, RELEASES.iter().copied())
                    .await?;
                fs::write(dir.join(format!("{channel}.json")), json)?;
            }
            Ok(())
        }
    }
}
