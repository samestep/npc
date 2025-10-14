# Contributing to `npc`

GitHub issues and pull requests are welcome!

Working in this repository is pretty straightforward: you just use the dev shell via [`nix develop`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-develop) or [direnv](https://direnv.net/). To build and run `npc` itself, you can either use [crane](https://crane.dev/) via [`nix run`](https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-run):

```sh
nix run . -- --help
```

Or you can just use [`cargo run`](https://doc.rust-lang.org/cargo/commands/cargo-run.html):

```sh
cargo run --release -- --help
```

Note that there is one hidden subcommand called `history` which is used to generate the JSON files under [`src`](src):

```sh
cargo run --release --all-features history src
```

Also, I used [markdown-toc](https://github.com/jonschlinkert/markdown-toc) to generate the table of contents in [`README.md`](README.md); you can run it like this:

```sh
nix-shell -p nodejs --run "npx markdown-toc --bullets='-' -i README.md"
```

This situation isn't ideal; I'll see if I can get markdown-toc into Nixpkgs to make this easier.
