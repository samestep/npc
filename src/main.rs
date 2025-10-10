use std::{fs, path::PathBuf};

use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3 as s3;
use clap::{Parser, Subcommand};

// We must specify the same region as the bucket or it responds with a `PermanentRedirect` error.
// This command can be used to check: `curl --head https://nix-releases.s3.amazonaws.com/`
const REGION: Region = Region::from_static("eu-west-1");

const UNSTABLE_CHANNELS: &[&str] = &["nixos-unstable", "nixos-unstable-small", "nixpkgs-unstable"];

// We exclude everything earlier because the bucket does not have `git-revision` objects for those.
const RELEASES: &[&str] = &[
    "16.09", "17.03", "17.09", "18.03", "18.09", "19.03", "19.09", "20.03", "20.09", "21.03",
    "21.05", "21.11", "22.05", "22.11", "23.05", "23.11", "24.05", "24.11", "25.05",
];

fn unstable_channel_prefix(channel: &str, release: &str) -> String {
    let prefix = match channel {
        "nixos-unstable" => "nixos/unstable/nixos",
        "nixos-unstable-small" => "nixos/unstable-small/nixos",
        "nixpkgs-unstable" => "nixpkgs/nixpkgs",
        _ => unimplemented!("{channel}"),
    };
    format!("{prefix}-{release}pre")
}

async fn get_client() -> s3::Client {
    // We use `no_credentials` to avoid authenticating because it's a public bucket.
    let config = aws_config::defaults(BehaviorVersion::latest())
        .no_credentials()
        .region(REGION)
        .load()
        .await;
    s3::Client::new(&config)
}

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    History { dir: PathBuf },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    match Cli::parse().command {
        Commands::History { dir } => {
            let client = get_client().await;
            for channel in UNSTABLE_CHANNELS {
                let filename = dir.join(format!("{channel}.txt"));
                let mut lines = String::new();
                for release in RELEASES {
                    let mut continuation_token = None;
                    loop {
                        let output = client
                            .list_objects_v2()
                            .bucket("nix-releases")
                            .prefix(unstable_channel_prefix(channel, release))
                            .delimiter("/")
                            .set_continuation_token(continuation_token)
                            .send()
                            .await?;
                        for prefix in output.common_prefixes() {
                            lines.push_str(prefix.prefix().unwrap());
                            lines.push('\n');
                        }
                        let token = output.next_continuation_token;
                        if token.is_none() {
                            break;
                        }
                        continuation_token = token;
                    }
                }
                fs::write(filename, lines)?;
            }
            Ok(())
        }
    }
}
