# `npc`

A CLI tool to access the history of Nixpkgs [channels](https://wiki.nixos.org/wiki/Channel_branches).

![example bisect session](example.svg)

<!-- toc -->

- [Motivation](#motivation)
- [Installation](#installation)
  - [Temporary](#temporary)
  - [Home Manager](#home-manager)
- [Usage](#usage)
  - [`npc fetch`](#npc-fetch)
  - [`npc clean`](#npc-clean)
  - [`npc status`](#npc-status)
  - [`npc log`](#npc-log)
  - [`npc checkout`](#npc-checkout)
  - [`npc bisect`](#npc-bisect)
    - [`npc bisect start`](#npc-bisect-start)
    - [`npc bisect bad` / `npc bisect new`](#npc-bisect-bad--npc-bisect-new)
    - [`npc bisect good` / `npc bisect old`](#npc-bisect-good--npc-bisect-old)
    - [`npc bisect reset`](#npc-bisect-reset)
- [Contributing](#contributing)
- [License](#license)

<!-- tocstop -->

## Motivation

The unstable Nixpkgs channels are an amazing way to get reproducible up-to-date versions of most software. However, things do break sometimes. Let's say you open a project you haven't touched in a couple months that uses the `nixpkgs-unstable` channel, and you run `nix flake update`. Something breaks. What do you do?

Without `npc`, you could either

1. come up with a simple proxy for what broke in your actual project, manually [`git bisect`](https://git-scm.com/docs/git-bisect) the [Nixpkgs repo](https://github.com/NixOS/nixpkgs) using the `--first-parent` flag, query the [Nix releases S3 bucket](https://releases.nixos.org/) to see what commits `nixpkgs-unstable` has actually pointed to in the past, bisect again to find the pair between which that earlier commit from `git bisect` fell, try that in your project, and repeat if your proxy check was insufficient; or,
2. revert your channel bump and keep using the months-old commit from before.

(Usually the second one.)

With `npc`, you can simply `npc bisect` directly in your own project to find what was the last time `nixpkgs-unstable` pointed to a commit that works for you, trying only commits to which that branch has pointed at some point in the past, and skipping everything else. Then commit the change to your lock file, and continue on your way.

## Installation

### Temporary

This repository provides a [Nix flake](https://wiki.nixos.org/wiki/Flakes), so if you have flakes enabled, the quickest way to use it is via the [`nix shell`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-env-shell.html) command:

```sh
nix shell github:samestep/npc
```

This starts a shell within your shell which has `npc` available on the `PATH`.

### Home Manager

If you use [Home Manager](https://github.com/nix-community/home-manager) and would like to do a more global installation, the easiest way is via the overlay provided by this flake; for instance, your `flake.nix` might look like this:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npc.url = "github:samestep/npc";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      home-manager,
      npc,
    }:
    flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages.homeConfigurations = {
        "username" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ npc.overlays.default ];
          };
          modules = [ ./home.nix ];
        };
      };
    });
}
```

Then you can add `npc` to your `home.nix` like this:

```nix
{ pkgs, ... }:
{
  home = {
    stateVersion = "26.05";
    username = "username";
    homeDirectory = "/home/username";
    packages = [
      pkgs.npc
    ];
  };
}
```

## Usage

`npc` provides several subcommands. To view them all, use the `--help` flag:

```sh
npc --help
```

You can also use `--help` on a specific subcommand to view more details about it:

```sh
npc log --help
```

The subcommand names are all borrowed from similar subcommands in [Git](https://git-scm.com/), so hopefully this CLI as a whole should feel familiar.

### `npc fetch`

This is the first command you need to run before you can do anything with `npc`. If you haven't run it before, it first clones the Nixpkgs repository, which can take a little while and will use up to a gigabyte of disk space.

All the other subcommands use this cache and don't try to update it themselves; for instance, if they see a commit that's not in the cache, they'll just give an error. You can always run `npc fetch` again to update the cache, which is faster once you've already done it at least once: in my experience, it takes less than a minute.

### `npc clean`

The cache shouldn't end up in a broken state, but if it ever does, you can use this command to delete the whole thing. It was probably a bug in `npc`, so please [let me know](CONTRIBUTING.md)!

Alternatively, if you've decided you don't want to use `npc` anymore and want to free up that gigabyte, this command works for that too.

### `npc status`

For all Nixpkgs branches in the previous release or newer, this command tells you when the branch was last updated, according to whenever you last ran `npc fetch`:

```
$ npc status
current local time is 2026-05-25 22:35:12 -0400
last fetched cache at 2026-05-25 22:34:09 -0400

master                2026-05-25 22:14:29 -0400
nixpkgs-unstable      2026-05-25 03:42:19 -0400
nixos-unstable-small  2026-05-25 10:43:04 -0400
nixos-unstable        2026-05-23 14:24:25 -0400
nixpkgs-26.05-darwin  2026-05-24 07:32:15 -0400
nixos-26.05-small     2026-05-25 01:24:44 -0400
nixos-26.05           2026-05-24 07:32:15 -0400
nixpkgs-25.11-darwin  2026-05-24 18:47:38 -0400
nixos-25.11-small     2026-05-25 04:08:50 -0400
nixos-25.11           2026-05-22 12:26:26 -0400
```

### `npc log`

This command prints the history of a Nixpkgs branch:

```
$ npc log nixpkgs-unstable -n10
f9d8b65950353691ab56561e7c73d2e1063d810b 2026-05-25 03:42:19 -0400
d849bb215dcdf71bce3e686839ccdb4219e84b2f 2026-05-23 23:33:00 -0400
3d8f0f3f72a6cd4d93d0ad13203f2ea1cb7e1456 2026-05-23 07:35:32 -0400
a0991c886dc83e6e9de01d5266e4842985b1850e 2026-05-22 14:39:28 -0400
6dedf69f94d03cbe7bdde106f2d4c23ae2a853bf 2026-05-21 21:51:30 -0400
4a29d733e8a7d5b824c3d8c958a946a9867b3eb2 2026-05-21 04:15:18 -0400
d99b013d5d1931ad77fe3912ed218170dec5d9a4 2026-05-20 02:38:13 -0400
d233902339c02a9c334e7e593de68855ad26c4cb 2026-05-15 14:21:44 -0400
8a1b0127302ea51e05bf4ea5a291743fac442406 2026-05-14 17:33:07 -0400
27198194e52129ccd8e845e7d5911911e7fea7d7 2026-05-14 08:36:40 -0400
```

If you don't pass `-n`/`--max-count` then there will be too many commits to fit on one screen, so the Git pager will be used to let you scroll through the list of commits, search for specific commits or dates, etc.

If you are in a directory that has a `flake.lock` or `devenv.lock` file, you don't need to specify the branch name explicitly; `npc` will determine it automatically:

```sh
npc log
```

There are a couple caveats to this, though:

- Currently `npc` only looks for flake inputs that look like `github:NixOS/nixpkgs` optionally followed by some branch name, and ignores other possible ways of specifying Nixpkgs. If your lock file refers to the Nixpkgs repo in a different way that you'd like `npc` to support, please [let me know](CONTRIBUTING.md)!

- If your lock file has multiple independent versions of Nixpkgs, even if they happen to currently point to the same commit, `npc` will not automatically choose one; you'll need to explicitly choose one yourself via the `--input` flag:

  ```sh
  npc log --input nixpkgs
  ```

- The `log` subcommand always just prints all the commits that a Nixpkgs branch has ever pointed to according to the `npc` cache, modulo the `-n`/`--max-count` argument. That is, it may even show commits newer than the commit you currently have in your lock file.

If you want to pick a specific lock file (for example, when both `flake.lock` and `devenv.lock` are present), pass `--lock-file PATH`.

### `npc checkout`

This command runs [`nix flake update`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-update) with [`--override-input`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-update#opt-override-input) to modify your lock file (by default `flake.lock`, or `devenv.lock` if `flake.lock` is missing):

```sh
# Fun fact: this is the most recent commit that was on
# all three of the `nixos-unstable`, `nixos-unstable-small`,
# and `nixpkgs-unstable` branches. It's from 2022.
npc checkout 1d7db1b9e4cf1ee075a9f52e5c36f7b9f4207502
```

Just like the `log` subcommand, `checkout` attempts to use your lock file to automatically infer the name of the flake input to modify. Similarly, if there are multiple instances of Nixpkgs in the lock file then it will ask you to explicitly specify one.

You can override the detected lock file with `--lock-file PATH`.

Other than saving you some typing, the primary difference between this and just running `nix flake update` yourself is that it checks whether the commit you give it is actually a commit that has been a tip of your Nixpkgs branch at some point in the past. If not, that's an error. The goal of this is to maintain consistency and reduce surprises: if your `flake.nix` says you're using the `nixos-unstable` branch, it'd be weird for your lock file to list a commit that has never been the tip of that branch.

### `npc bisect`

This is like `git bisect`, except instead finding the commit in your repository that introduced a bug, it finds the most recent Nixpkgs commit that doesn't have the bug but was at some point the tip of a given branch.

The easiest way to use this is in a flake where it can automatically do the equivalent of `npc checkout` while narrowing in on a specific commit. But you can also do it without a lock file if you specify a branch; the difference is just that you'll need to explicity specify each commit, rather than `npc` automatically reading the current commit from the lock file at each step.

Note that if you are currently bisecting in a given directory, running `npc status` will also print the current bisection status.

#### `npc bisect start`

Just like `checkout`, if you have exactly one Nixpkgs input in the lock file then you don't need to specify any further information:

```sh
npc bisect start
```

If you have multiple Nixpkgs inputs then you need to specify one via `--input` on this command. Since bisection status is stored per directory, you only need to specify that when you start bisecting, and not each time you mark a commit as `bad` or `good`.

As with the other commands, you can point at a specific lock file using `--lock-file PATH`.

Either way, that will start bisecting in "flake mode"; if you instead want to bisect outside the context of any flake, simply specify a branch:

```sh
npc bisect start nixpkgs-unstable
```

Note that if you specify a branch, the bisection will not use "flake mode" even if you also have a unique Nixpkgs input with that branch in the lock file. Specifying a branch here will cause the other `npc bisect` commands to ignore the lock file entirely.

#### `npc bisect bad` / `npc bisect new`

These subcommands are aliases of each other, just like with Git. They mark a Nixpkgs commit as being broken, or more generally, as being after the change that we are trying to pinpoint.

If you are bisecting in "flake mode" then you can run this with no argument, and `npc` will read the current commit from your lock file according to whatever flake input was determined when you first started bisecting:

```sh
npc bisect bad
```

However, you can always instead provide a commit explicitly.

#### `npc bisect good` / `npc bisect old`

Similarly, these two subcommands let you mark a commit as not broken, or more generally, as being before the change happened.

Once you have specified at least one "bad" commit and at least one "good" commit, `npc` will determine the midpoint commit in the history of the branch being bisected, according to the last good and first bad commits it has seen so far. If you are in "flake mode" then it will also do the equivalent of `npc checkout` with that midpoint commit, every time you mark any further commits as "bad" or "good".

#### `npc bisect reset`

Once you finish bisecting, all the state is still stored. This is to allow you to check back on the end result via the `status` subcommand if you'd like. Then whenever you're ready, you can go ahead and delete the state for the current directory:

```sh
npc bisect reset
```

The main difference from `git bisect reset` is that this only deletes the bisection state, and does not make any further modifications to your lock file. That is, it does not bring you back to the original Nixpkgs commit you were on before you started bisecting; you'd need to do that manually if you didn't want to end up on the commit that `npc bisect` found.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## License

`npc` is licensed under the [MIT License](LICENSE).
