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
    - [`npc bisect bad`](#npc-bisect-bad)
    - [`npc bisect good`](#npc-bisect-good)
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

If you use [Home Manager](https://github.com/nix-community/home-manager) and would like to do a more global installation, you can add `npc` to your `home.nix` like this:

```nix
{ pkgs, npc, ... }:
{
  home = {
    stateVersion = "25.05";
    username = "username";
    homeDirectory = "/home/username";
    packages = [
      npc.packages.${pkgs.system}.default
    ];
  };
}
```

This is assuming you configure Home Manager via a flake, in which case you'd pass `npc` to `home.nix` via `extraSpecialArgs`:

<details>
<summary><code>flake.nix</code></summary>

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npc = {
      url = "github:samestep/npc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
          pkgs = import nixpkgs { inherit system; };
          modules = [ ./home.nix ];
          extraSpecialArgs = { inherit npc; };
        };
      };
    });
}
```

</details>

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

### `npc clean`

### `npc status`

### `npc log`

### `npc checkout`

### `npc bisect`

#### `npc bisect start`

#### `npc bisect bad`

#### `npc bisect good`

#### `npc bisect reset`

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

## License

`npc` is licensed under the [MIT License](LICENSE).
