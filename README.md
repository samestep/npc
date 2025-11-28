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

With `npc`, you can simply `npc bisect` directly in your own project to find what was the last time `nixpkgs-unstable` pointed to a commit that works for you, trying only commits to which that branch has pointed at some point in the past, and skipping everything else. Then commit the change to `flake.lock`, and continue on your way.

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
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
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
      homeConfigurations = {
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
    stateVersion = "25.05";
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
current local time is 2025-11-28 14:48:17 -0500
last fetched cache at 2025-11-28 14:47:53 -0500

master                2025-11-28 14:12:53 -0500
nixpkgs-unstable      2025-11-27 02:58:14 -0500
nixos-unstable-small  2025-11-27 19:11:30 -0500
nixos-unstable        2025-11-27 06:14:36 -0500
nixpkgs-25.11-darwin  2025-11-26 11:09:25 -0500
nixos-25.11-small     2025-11-27 20:42:38 -0500
nixos-25.11           2025-11-26 11:09:25 -0500
nixpkgs-25.05-darwin  2025-11-27 09:19:47 -0500
nixos-25.05-small     2025-11-28 12:35:20 -0500
nixos-25.05           2025-11-23 20:37:40 -0500
```

### `npc log`

This command prints the history of a Nixpkgs branch:

```
$ npc log nixpkgs-unstable -n10
0d59e0290eefe0f12512043842d7096c4070f30e 2025-11-27 02:58:14 -0500
5c46f3bd98147c8d82366df95bbef2cab3a967ea 2025-11-26 09:39:26 -0500
bb813de6d2241bcb1b5af2d3059f560c66329967 2025-11-26 01:22:50 -0500
dc205f7b4fdb04c8b7877b43edb7b73be7730081 2025-11-25 09:41:04 -0500
ee09932cedcef15aaf476f9343d1dea2cb77e261 2025-11-23 16:50:36 -0500
878e468e02bfabeda08c79250f7ad583037f2227 2025-11-22 05:07:53 -0500
9fe0d00db1794fe493677b1abe9ca6d08965f4d1 2025-11-21 03:16:52 -0500
a8d610af3f1a5fb71e23e08434d8d61a466fc942 2025-11-20 01:07:48 -0500
6f374686605df381de8541c072038472a5ea2e2d 2025-11-18 06:19:29 -0500
0ade817efdde79cbc46b624c849027335ebc25c5 2025-11-17 22:14:24 -0500
```

If you don't pass `-n`/`--max-count` then there will be too many commits to fit on one screen, so the Git pager will be used to let you scroll through the list of commits, search for specific commits or dates, etc.

If you are in a directory that has a `flake.lock` file, you don't need to specify the branch name explicitly; `npc` will determine it automatically:

```sh
npc log
```

There are a couple caveats to this, though:

- Currently `npc` only looks for flake inputs that look like `github:NixOS/nixpkgs` optionally followed by some branch name, and ignores other possible ways of specifying Nixpkgs. If your flake refers to the Nixpkgs repo in a different way that you'd like `npc` to support, please [let me know](CONTRIBUTING.md)!

- If your `flake.lock` file has multiple independent versions of Nixpkgs, even if they happen to currently point to the same commit, `npc` will not automatically choose one; you'll need to explicitly choose one yourself via the `--input` flag:

  ```sh
  npc log --input nixpkgs
  ```

- The `log` subcommand always just prints all the commits that a Nixpkgs branch has ever pointed to according to the `npc` cache, modulo the `-n`/`--max-count` argument. That is, it may even show commits newer than the commit you currently have in your `flake.lock`.

### `npc checkout`

This command runs [`nix flake update`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-update) with [`--override-input`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-update#opt-override-input) to modify your `flake.lock` file:

```sh
# Fun fact: this is the most recent commit that was on
# all three of the `nixos-unstable`, `nixos-unstable-small`,
# and `nixpkgs-unstable` branches. It's from 2022.
npc checkout 1d7db1b9e4cf1ee075a9f52e5c36f7b9f4207502
```

Just like the `log` subcommand, `checkout` attempts to use your `flake.lock` to automatically infer the name of the flake input to modify. Similarly, if there are multiple instances of Nixpkgs in `flake.lock` then it will ask you to explicitly specify one.

Other than saving you some typing, the primary difference between this and just running `nix flake update` yourself is that it checks whether the commit you give it is actually a commit that has been a tip of your Nixpkgs branch at some point in the past. If not, that's an error. The goal of this is to maintain consistency and reduce surprises: if your `flake.nix` says you're using the `nixos-unstable` branch, it'd be weird for your `flake.lock` to list a commit that has never been the tip of that branch.

### `npc bisect`

This is like `git bisect`, except instead finding the commit in your repository that introduced a bug, it finds the most recent Nixpkgs commit that doesn't have the bug but was at some point the tip of a given branch.

The easiest way to use this is in a flake where it can automatically do the equivalent of `npc checkout` while narrowing in on a specific commit. But you can also do it without a `flake.lock` if you specify a branch; the difference is just that you'll need to explicity specify each commit, rather than `npc` automatically reading the current commit from `flake.lock` at each step.

Note that if you are currently bisecting in a given directory, running `npc status` will also print the current bisection status.

#### `npc bisect start`

Just like `checkout`, if you have exactly one Nixpkgs input in `flake.lock` then you don't need to specify any further information:

```sh
npc bisect start
```

If you have multiple Nixpkgs inputs then you need to specify one via `--input` on this command. Since bisection status is stored per directory, you only need to specify that when you start bisecting, and not each time you mark a commit as `bad` or `good`.

Either way, that will start bisecting in "flake mode"; if you instead want to bisect outside the context of any flake, simply specify a branch:

```sh
npc bisect start nixpkgs-unstable
```

Note that if you specify a branch, the bisection will not use "flake mode" even if you also have a unique Nixpkgs input with that branch in `flake.lock`. Specifying a branch here will cause the other `npc bisect` commands to ignore `flake.lock` entirely.

#### `npc bisect bad` / `npc bisect new`

These subcommands are aliases of each other, just like with Git. They mark a Nixpkgs commit as being broken, or more generally, as being after the change that we are trying to pinpoint.

If you are bisecting in "flake mode" then you can run this with no argument, and `npc` will read the current commit from your `flake.lock` according to whatever flake input was determined when you first started bisecting:

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

The main difference from `git bisect reset` is that this only deletes the bisection state, and does not make any further modifications to your `flake.lock`. That is, it does not bring you back to the original Nixpkgs commit you were on before you started bisecting; you'd need to do that manually if you didn't want to end up on the commit that `npc bisect` found.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## License

`npc` is licensed under the [MIT License](LICENSE).
