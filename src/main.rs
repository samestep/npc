use std::{
    cell::Cell,
    collections::VecDeque,
    env,
    fmt::{self, Write as _},
    fs,
    io::{self, BufRead, Write as _},
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str::FromStr,
    sync::atomic::{AtomicU64, Ordering},
    time::Duration,
};

use anyhow::{Context, anyhow, bail};
use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3 as s3;
use clap::{Parser, Subcommand};
use futures::future::try_join_all;
use indexmap::IndexMap;
use indicatif::ProgressBar;
use percent_encoding::{NON_ALPHANUMERIC, percent_encode};
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Visitor};
use thiserror::Error;

/// The name of this program.
const NAME: &str = env!("CARGO_PKG_NAME");

const GIT: &str = env!("GIT_BIN");
const NIX: &str = env!("NIX_BIN");

const REMOTES: &str = "
origin\thttps://github.com/NixOS/nixpkgs.git (fetch) [tree:0]
origin\thttps://github.com/NixOS/nixpkgs.git (push)
"
.trim_ascii_start();

const BUCKET: &str = "nix-releases";

// We must specify the same region as the bucket or it responds with a `PermanentRedirect` error.
// This command can be used to check: `curl --head https://nix-releases.s3.amazonaws.com/`
const REGION: Region = Region::from_static("eu-west-1");

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

fn git() -> Command {
    let mut cmd = Command::new(GIT);
    // Disable Git config: https://git-scm.com/docs/git/2.47.0#Documentation/git.txt-GITCONFIGGLOBAL
    cmd.env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_SYSTEM", "/dev/null");
    // For example, without this, the `git remote` check in `Cache::new` would fail if someone uses
    // `pushInsteadOf` to push with SSH instead of HTTPS for GitHub:
    // https://git-scm.com/docs/git-config/2.47.0#Documentation/git-config.txt-urlbasepushInsteadOf
    cmd
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Sha([u8; 20]);

impl fmt::Display for Sha {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for byte in self.0 {
            write!(&mut s, "{:02x}", byte)?;
        }
        f.pad(&s)
    }
}

#[derive(Error, Debug)]
#[error("invalid Git SHA")]
struct ParseShaError;

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
        Ok(Self(array))
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
        formatter.pad("a Git SHA")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        v.parse().map_err(E::custom)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Release {
    year: u8,
    month: u8,
}

impl fmt::Display for Release {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(&format!("{:02}.{:02}", self.year, self.month))
    }
}

#[derive(Error, Debug)]
#[error("invalid NixOS release")]
struct ParseReleaseError;

impl FromStr for Release {
    type Err = ParseReleaseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^(\d\d)\.(\d\d)$").unwrap();
        let caps = re.captures(s).ok_or(ParseReleaseError)?;
        let year = caps[1].parse().unwrap();
        let month = caps[2].parse().unwrap();
        Ok(Release { year, month })
    }
}

#[derive(Clone, Copy)]
enum Branch {
    Master,
    NixpkgsUnstable,
    NixosUnstableSmall,
    NixosUnstable,
    NixpkgsStableDarwin(Release),
    NixosStableSmall(Release),
    NixosStable(Release),
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Branch::Master => f.pad("master"),
            Branch::NixpkgsUnstable => f.pad("nixpkgs-unstable"),
            Branch::NixosUnstableSmall => f.pad("nixos-unstable-small"),
            Branch::NixosUnstable => f.pad("nixos-unstable"),
            Branch::NixpkgsStableDarwin(release) => f.pad(&format!("nixpkgs-{release}-darwin")),
            Branch::NixosStableSmall(release) => f.pad(&format!("nixos-{release}-small")),
            Branch::NixosStable(release) => f.pad(&format!("nixos-{release}")),
        }
    }
}

#[derive(Error, Debug)]
#[error("invalid Nixpkgs branch")]
struct ParseBranchError;

fn stable_branch(re: &str, haystack: &str) -> Option<Result<Release, ParseBranchError>> {
    Regex::new(re)
        .unwrap()
        .captures(haystack)
        .map(|caps| caps[1].parse().map_err(|_| ParseBranchError))
}

impl FromStr for Branch {
    type Err = ParseBranchError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "master" {
            Ok(Branch::Master)
        } else if s == "nixpkgs-unstable" {
            Ok(Branch::NixpkgsUnstable)
        } else if s == "nixos-unstable-small" {
            Ok(Branch::NixosUnstableSmall)
        } else if s == "nixos-unstable" {
            Ok(Branch::NixosUnstable)
        } else if let Some(release) = stable_branch(r"^nixpkgs-(.*)-darwin$", s) {
            Ok(Branch::NixpkgsStableDarwin(release?))
        } else if let Some(release) = stable_branch(r"^nixos-(.*)-small$", s) {
            Ok(Branch::NixosStableSmall(release?))
        } else if let Some(release) = stable_branch(r"^nixos-(.*)$", s) {
            Ok(Branch::NixosStable(release?))
        } else {
            Err(ParseBranchError)
        }
    }
}

impl Serialize for Branch {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for Branch {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(BranchVisitor)
    }
}

struct BranchVisitor;

impl<'de> Visitor<'de> for BranchVisitor {
    type Value = Branch;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.pad("a Nixpkgs branch")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        v.parse().map_err(E::custom)
    }
}

impl Branch {
    fn iter(releases: &[Release]) -> impl Iterator<Item = Self> {
        let mut branches = vec![
            Branch::Master,
            Branch::NixpkgsUnstable,
            Branch::NixosUnstableSmall,
            Branch::NixosUnstable,
        ];
        for &release in releases.iter().rev() {
            branches.extend_from_slice(&[
                Branch::NixpkgsStableDarwin(release),
                Branch::NixosStableSmall(release),
                Branch::NixosStable(release),
            ]);
        }
        branches.into_iter()
    }

    fn releases(self, releases: &[Release]) -> &[Release] {
        match self {
            Self::Master => &[],
            Self::NixpkgsUnstable | Self::NixosUnstableSmall | Self::NixosUnstable => releases,
            Self::NixpkgsStableDarwin(release)
            | Self::NixosStableSmall(release)
            | Self::NixosStable(release) => {
                let Some(i) = releases.iter().position(|&item| item == release) else {
                    return &[];
                };
                &releases[i..i + 1]
            }
        }
    }

    fn prefix(self, release: Release) -> String {
        match self {
            Self::Master => panic!("master branch is not tracked in S3"),
            Self::NixpkgsUnstable => format!("nixpkgs/nixpkgs-{release}pre"),
            Self::NixosUnstableSmall => format!("nixos/unstable-small/nixos-{release}pre"),
            Self::NixosUnstable => format!("nixos/unstable/nixos-{release}pre"),
            Self::NixpkgsStableDarwin(actual) => {
                assert_eq!(release, actual);
                format!("nixpkgs/{release}-darwin/")
            }
            Self::NixosStableSmall(actual) => {
                assert_eq!(release, actual);
                format!("nixos/{release}-small/")
            }
            Self::NixosStable(actual) => {
                assert_eq!(release, actual);
                format!("nixos/{release}/")
            }
        }
    }

    fn key(self) -> CacheKey<'static> {
        CacheKey::Branch(self)
    }
}

enum CacheKey<'a> {
    LastFetched,
    Releases,
    Git,
    Branch(Branch),
    Bisect(&'a Path),
}

impl CacheKey<'_> {
    fn name(self) -> String {
        match self {
            Self::LastFetched => "last-fetched.txt".to_owned(),
            Self::Releases => "releases.txt".to_owned(),
            Self::Git => "nixpkgs.git".to_owned(),
            Self::Branch(Branch::Master) => "master.dat".to_owned(),
            Self::Branch(channel) => format!("{channel}.json"),
            Self::Bisect(path) => {
                assert!(path.is_absolute());
                // We use percent encoding to turn an entire path into a single pathname component.
                let encoded = percent_encode(path.as_os_str().as_bytes(), NON_ALPHANUMERIC);
                format!("bisect/{encoded}.json")
            }
        }
    }
}

type History = Vec<Sha>;

struct PartialCache {
    dir: PathBuf,
    missing_git: bool,
}

struct Cache {
    dir: PathBuf,
    last_fetched: String,
    releases: Vec<Release>,
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

        let releases = match fs::read_to_string(dir.join(CacheKey::Releases.name())) {
            Ok(contents) => Some(
                contents
                    .lines()
                    .map(|line| Ok(line.to_owned().parse()?))
                    .collect::<anyhow::Result<Vec<Release>>>()?,
            ),
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => None,
                _ => bail!(err),
            },
        };
        if releases.is_none() {
            missing_other = true;
        }

        let git_dir = dir.join(CacheKey::Git.name());
        if fs::exists(&git_dir)? {
            let output = git()
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

        for branch in Branch::iter(releases.as_deref().unwrap_or_default()) {
            let key = branch.key();
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
                releases: releases.unwrap(),
            }))
        }
    }

    fn path(&self, key: CacheKey) -> PathBuf {
        self.dir.join(key.name())
    }

    fn git(&self) -> Command {
        let mut cmd = git();
        // Because we did a blobless clone, some commands that wouldn't normally need network access
        // might try to lazily fetch objects. We consider it a bug for subcommands other than
        // `fetch` to access the network (modulo `nix flake update` as used by the `checkout` and
        // `bisect` subcommands), so here we disallow that. Unfortunately this seems to cause Git to
        // hang rather than simply exiting with an error, but it's better than nothing.
        cmd.args(["--no-lazy-fetch", "-C"])
            .arg(self.path(CacheKey::Git));
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
            Branch::Master => {
                let bytes = fs::read(self.path(branch.key()))?;
                let (chunks, rest) = bytes.as_chunks();
                if !rest.is_empty() {
                    bail!("broken cache for Nixpkgs master branch");
                }
                Ok(chunks.iter().copied().map(Sha).collect())
            }
            channel => {
                let history: IndexMap<Sha, String> =
                    serde_json::from_str(&fs::read_to_string(self.path(channel.key()))?)?;
                Ok(history.into_keys().collect())
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
        let Some(index) = commits.iter().position(|&sha| sha == rev) else {
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
                let Some(&sha) = commits.get(index_good.midpoint(index_bad)) else {
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

/// Query the nix-channels bucket to determine the next release.
async fn prerelease() -> anyhow::Result<Release> {
    let config = aws_config::defaults(BehaviorVersion::latest())
        .no_credentials()
        .region("us-east-1")
        .load()
        .await;
    let s3 = s3::Client::new(&config);
    let unstable = [
        Branch::NixpkgsUnstable,
        Branch::NixosUnstableSmall,
        Branch::NixosUnstable,
    ];
    let outputs = try_join_all(unstable.map(|channel| {
        s3.get_object()
            .bucket("nix-channels")
            .key(channel.to_string())
            .send()
    }))
    .await?;
    let mut release = None;
    let re = Regex::new(r"(\d\d\.\d\d)pre\d+\.\w+$").unwrap();
    for (output, channel) in outputs.into_iter().zip(unstable) {
        let Some(url) = output.website_redirect_location else {
            bail!("no redirect URL found for {channel}");
        };
        let Some(caps) = re.captures(&url) else {
            bail!("failed to find prerelease version in {url}");
        };
        let rel = caps[1].parse().unwrap();
        if let Some(other) = release
            && rel != other
        {
            bail!("can't decide between prerelease versions {other} and {rel}");
        }
        release = Some(rel);
    }
    Ok(release.unwrap())
}

struct PrefixId {
    re: Regex,
}

impl PrefixId {
    fn new() -> Self {
        let re = Regex::new(r"(\d+)\.\w+/$").unwrap();
        Self { re }
    }

    fn get(&self, prefix: &str) -> Option<u128> {
        let caps = self.re.captures(prefix)?;
        let id = caps[1].parse().ok()?;
        Some(id)
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

    async fn releases(&self) -> anyhow::Result<Vec<Release>> {
        let mut releases = Vec::new();
        let re = Regex::new(r"^nixos/(\d\d\.\d\d)/$").unwrap();
        let mut continuation_token = None;
        loop {
            let output = self
                .s3
                .list_objects_v2()
                .bucket(BUCKET)
                .prefix("nixos/")
                .delimiter("/")
                .set_continuation_token(continuation_token)
                .send()
                .await?;
            let prefixes = output
                .common_prefixes
                .unwrap_or_default()
                .into_iter()
                .map(|item| item.prefix.ok_or_else(|| anyhow!("missing prefix")))
                .collect::<anyhow::Result<Vec<String>>>()?;
            for prefix in prefixes {
                if let Some(caps) = re.captures(&prefix) {
                    let release: Release = caps[1].parse().unwrap();
                    // We omit earlier releases: the bucket has no `git-revision` objects for them.
                    let oldest = Release { year: 16, month: 9 };
                    if release.year > 16 || release == oldest {
                        releases.push(release);
                    }
                }
            }
            match output.next_continuation_token {
                Some(token) => continuation_token = Some(token),
                None => break,
            };
        }
        releases.push(prerelease().await?);
        Ok(releases)
    }

    async fn git_revisions(
        &self,
        mut prefixes: VecDeque<String>,
        mut callback: impl FnMut(Sha, String),
    ) -> anyhow::Result<()> {
        while !prefixes.is_empty() {
            let mut cmd = self.cache.git();
            cmd.arg("rev-parse");
            // We cap the number of commits we try to ask Git for at once, because otherwise the
            // retries could potentially cause this function to become quadratic instead of linear.
            for prefix in prefixes.iter().take(100) {
                let dot = prefix.rfind(".").ok_or_else(|| anyhow!("no dot"))?;
                let short = &prefix[dot + 1..prefix.len() - 1];
                cmd.arg(short);
            }
            let output = cmd.output()?;
            for line in String::from_utf8(output.stdout)?.lines() {
                let Some(prefix) = prefixes.pop_front() else {
                    bail!("unexpected extra Git output");
                };
                let sha = match line.parse() {
                    Ok(sha) => sha,
                    Err(_) => {
                        let key = format!("{prefix}git-revision");
                        let output = self.s3.get_object().bucket(BUCKET).key(key).send().await?;
                        String::from_utf8(output.body.collect().await?.to_vec())?.parse()?
                    }
                };
                callback(sha, prefix);
            }
        }
        Ok(())
    }

    async fn list_revisions(
        &self,
        channel: Branch,
        release: Release,
        mut callback: impl FnMut(Sha, String),
    ) -> anyhow::Result<()> {
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
            let prefixes = output
                .common_prefixes
                .unwrap_or_default()
                .into_iter()
                .map(|item| item.prefix.ok_or_else(|| anyhow!("missing prefix")))
                .collect::<anyhow::Result<VecDeque<String>>>()?;
            self.git_revisions(prefixes, &mut callback).await?;
            match output.next_continuation_token {
                Some(token) => continuation_token = Some(token),
                None => break,
            };
        }
        Ok(())
    }

    async fn channel_json(
        &self,
        channel: Branch,
        releases: impl Iterator<Item = Release>,
        inc: impl Fn(),
    ) -> anyhow::Result<String> {
        let cell = Cell::new(IndexMap::new());
        try_join_all(releases.map(|release| {
            self.list_revisions(channel, release, |sha, prefix| {
                let mut pairs = cell.take();
                pairs.insert(sha, prefix);
                let tmp = cell.replace(pairs);
                assert!(tmp.is_empty());
                inc();
            })
        }))
        .await?;
        let mut pairs = cell.into_inner();
        // We handled the list of releases concurrently, but even if we didn't, S3 lists things in
        // lexicographical order, which doesn't necessarily match up with the chronological order we
        // want. First, the beta versions of stable channels get listed last, even though they
        // actually come first chronologically. Second, the integer ID that comes before the commit
        // hash prefix can have an arbitrary number of digits, which causes the ordering to be wrong
        // when e.g. an ID has few digits but a high leading digit.
        let key = PrefixId::new();
        for prefix in pairs.values() {
            if key.get(prefix).is_none() {
                bail!("failed to find ID in prefix {prefix}");
            }
        }
        pairs.sort_by_key(|_, prefix| key.get(prefix).unwrap());
        let json = push_newline(serde_json::to_string_pretty(&pairs)?);
        Ok(json)
    }

    async fn fetch_channels<I: IntoIterator<Item = Release>>(
        &self,
        releases: impl Fn(Branch) -> I,
        callback: impl Copy + Fn(Branch, String) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let count = AtomicU64::new(0);
        let spinner = ProgressBar::new_spinner();
        spinner.set_message("fetching channels...");
        spinner.enable_steady_tick(Duration::from_millis(100));
        try_join_all(Branch::iter(&self.cache.releases).filter_map(|branch| {
            let mut iterator = releases(branch).into_iter().peekable();
            iterator.peek()?; // Ignore this branch if there's nothing to fetch for it.
            let (count, spinner) = (&count, &spinner);
            Some(async move {
                let json = self
                    .channel_json(branch, iterator, || {
                        let n = count.fetch_add(1, Ordering::Relaxed);
                        spinner.set_message(format!("fetching channels... {n} commits"));
                    })
                    .await?;
                callback(branch, json)?;
                Ok::<_, anyhow::Error>(())
            })
        }))
        .await?;
        spinner.finish();
        Ok(())
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
#[serde(untagged)]
enum FlakePath {
    Direct(String),
    Indirect(Vec<String>),
}

#[derive(Debug, Deserialize)]
struct FlakeNode {
    original: Option<FlakeOriginal>,
    locked: Option<FlakeLocked>,
    inputs: Option<IndexMap<String, FlakePath>>,
}

#[derive(Debug, Deserialize)]
struct FlakeLock {
    version: usize,
    nodes: IndexMap<String, FlakeNode>,
    root: String,
}

impl FlakeLock {
    fn new() -> anyhow::Result<Option<Self>> {
        let err = match fs::read_to_string("flake.lock") {
            Ok(json) => match serde_json::from_str::<FlakeLock>(&json) {
                Ok(flake_lock) => {
                    let v = flake_lock.version;
                    if v != 7 {
                        bail!("expected `flake.lock` format version 7 but this one is version {v}");
                    }
                    return Ok(Some(flake_lock));
                }
                Err(err) => anyhow!(err),
            },
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => return Ok(None),
                _ => anyhow!(err),
            },
        };
        Err(err.context("`flake.lock` broken"))
    }

    fn resolve(&self, path: &[impl AsRef<str>]) -> anyhow::Result<&str> {
        let mut key: &str = &self.root;
        for name in path {
            let name = name.as_ref();
            let Some(FlakeNode {
                inputs: Some(inputs),
                ..
            }) = self.nodes.get(key)
            else {
                bail!("could not find inputs for node {key} in `flake.lock`");
            };
            match inputs.get(name) {
                None => bail!("node {key} in `flake.lock` has no input named {name}"),
                Some(FlakePath::Direct(new_key)) => key = new_key,
                Some(FlakePath::Indirect(path)) => key = self.resolve(path)?,
            }
        }
        Ok(key)
    }
}

struct FlakeInput {
    name: String,
    rev: Sha,
}

fn filter_node(node: &FlakeNode) -> Option<(Branch, Sha)> {
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
    ) = (&node.original, &node.locked)
        && let Ok(branch) = branch.as_deref().unwrap_or("master").parse()
        && owner_original == "NixOS"
        && repo_original == "nixpkgs"
        && owner_locked == "NixOS"
        && repo_locked == "nixpkgs"
    {
        Some((branch, *rev))
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
        (None, None, None) => bail!("no branch specified and no `flake.lock` found"),
        (Some(branch), None, _) => Ok((branch, None)),
        (None, Some(name), Some(flake_lock)) => {
            let parts: Vec<_> = name.split('/').collect();
            let key = flake_lock.resolve(&parts)?;
            let Some(node) = flake_lock.nodes.get(key) else {
                bail!("no node named {key} in `flake.lock`");
            };
            let Some((bran, rev)) = filter_node(node) else {
                bail!("expected Nixpkgs in flake input named {name}");
            };
            Ok((bran, Some(FlakeInput { name, rev })))
        }
        (None, None, Some(flake_lock)) => {
            let mut paths = Vec::new();
            let mut stack = vec![(None, &flake_lock.root)];
            while let Some((path, key)) = stack.pop() {
                let index = paths.len();
                paths.push((path, key));
                if let Some(FlakeNode {
                    inputs: Some(inputs),
                    ..
                }) = flake_lock.nodes.get(key)
                {
                    for (input, subpath) in inputs {
                        if let FlakePath::Direct(subkey) = subpath {
                            stack.push((Some((index, input)), subkey));
                        }
                    }
                }
            }
            let mut it = paths.iter().enumerate().filter_map(|(index, (_, key))| {
                let (branch, rev) = filter_node(flake_lock.nodes.get(*key)?)?;
                Some((index, branch, rev))
            });
            let Some((mut index, branch, rev)) = it.next() else {
                bail!("no Nixpkgs input found in `flake.lock`");
            };
            if it.next().is_some() {
                bail!("multiple Nixpkgs inputs in `flake.lock`; please specify `--input`");
            }
            let mut parts = Vec::<&str>::new();
            while let (Some((parent, part)), _) = paths[index] {
                parts.push(part);
                index = parent;
            }
            parts.reverse();
            let name = parts.join("/");
            Ok((branch, Some(FlakeInput { name, rev })))
        }
    }
}

fn flake_update(name: &str, sha: Sha) -> anyhow::Result<()> {
    // Even if the user already has flakes enabled, our hardcoded Nix path may bypass that e.g. on
    // Determinate Nix, so we pass flags to enable them regardless.
    let status = Command::new(NIX)
        .args([
            "--extra-experimental-features",
            "nix-command flakes",
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
        /// Nixpkgs channel name, or `master`
        branch: Option<String>,

        /// Slash-separated flake input name
        #[clap(long, value_name = "NAME")]
        input: Option<String>,

        /// Limit the number of shown commits
        #[clap(short = 'n', long, value_name = "NUMBER")]
        max_count: Option<usize>,
    },

    /// Set a different Nixpkgs commit in `flake.lock`
    Checkout {
        /// Nixpkgs Git revision
        rev: String,

        /// Slash-separated flake input name
        #[clap(long, value_name = "NAME")]
        input: Option<String>,
    },

    /// Use binary search to find the newest commit of a branch before a bug
    Bisect {
        #[command(subcommand)]
        bisect: Bisect,
    },
}

#[derive(Subcommand)]
enum Bisect {
    /// Start bisecting in this directory
    Start {
        /// Nixpkgs channel name, or `master`
        branch: Option<String>,

        /// Slash-separated flake input name
        #[clap(long, value_name = "NAME")]
        input: Option<String>,
    },

    /// Mark this commit as "bad"
    #[command(visible_alias = "new")]
    Bad {
        /// Nixpkgs Git revision
        rev: Option<String>,
    },

    /// Mark this commit as "good"
    #[command(visible_alias = "old")]
    Good {
        /// Nixpkgs Git revision
        rev: Option<String>,
    },

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
                    let cache = Cache {
                        dir,
                        last_fetched,
                        releases: Vec::new(),
                    };
                    if missing_git {
                        let repo = "https://github.com/NixOS/nixpkgs.git";
                        // We shouldn't need any trees or blobs, only history information.
                        let status = git()
                            .args(["clone", "--mirror", "--filter=tree:0", repo])
                            .arg(cache.path(CacheKey::Git))
                            .status()?;
                        if !status.success() {
                            bail!("failed to clone {repo}");
                        }
                    }
                    cache
                }
            };

            let mut remote = Remote::new(cache).await;

            remote.cache.releases = {
                let releases = remote.releases().await?;
                let mut lines = String::new();
                for release in &releases {
                    writeln!(&mut lines, "{release}")?;
                }
                fs::write(remote.cache.path(CacheKey::Releases), lines)?;
                releases
            };

            remote
                .fetch_channels(
                    |branch| branch.releases(&remote.cache.releases).iter().copied(),
                    |branch, json| Ok(fs::write(remote.cache.path(branch.key()), json)?),
                )
                .await?;

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

            // List `master` commits only after `git fetch` so that we don't miss any new ones.
            let output = remote
                .cache
                .git()
                .args(["log", "--first-parent", "--format=%H"])
                .output()?;
            if !output.status.success() {
                bail!("failed to list commits from Nixpkgs master branch");
            }
            let mut shas = Vec::new();
            for line in output.stdout.lines() {
                let sha: Sha = line?.parse()?;
                shas.push(sha.0);
            }
            shas.reverse();
            let bytes = shas.into_flattened();
            fs::write(remote.cache.path(CacheKey::Branch(Branch::Master)), bytes)?;

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
            let releases = cache.releases.len();
            // Don't try to print stable branches for the next release since those don't exist yet,
            // and don't print stable branches older than the previous release since those are no
            // longer being updated.
            for branch in Branch::iter(&cache.releases[releases - 3..releases - 1]) {
                assert!(branch.to_string().len() <= width);
                match cache.branch(branch)?.last() {
                    None => println!("{branch}"),
                    Some(sha) => {
                        // We must use `log` instead of `show --no-patch` because the latter would
                        // attempt to access the network.
                        let output = cache
                            .git()
                            .args(["log", "--max-count=1", "--date=iso-local", "--format=%cd"])
                            .arg(sha.to_string())
                            .output()?;
                        if !output.status.success() {
                            bail!("failed to show Git commit date");
                        }
                        let date = pop_newline(String::from_utf8(output.stdout)?)?;
                        println!("{branch:<width$} {date}");
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
            let commits = cache.branch(branch)?;
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
            // We can't just forward `--max-count` straight through to Git, because that causes it
            // to ignore the `--no-walk` argument.
            for sha in commits
                .iter()
                .rev()
                .take(max_count.unwrap_or(commits.len()))
            {
                let stdin = child.stdin.as_mut().unwrap();
                writeln!(stdin, "{sha}")?;
            }
            child.wait()?;
            Ok(())
        }

        (Ok(cache), Commands::Checkout { rev, input }) => {
            let sha = cache.sha(&rev)?;
            let (branch, Some(input)) = resolve(None, input)? else {
                bail!("could not determine flake input to update");
            };
            if !cache.branch(branch)?.contains(&sha) {
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
    }
}
