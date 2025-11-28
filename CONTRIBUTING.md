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

There are a couple lints you can run along with building the tool itself:

```sh
nix flake check
```

One lint uses [markdown-toc](https://github.com/jonschlinkert/markdown-toc) to generate the table of contents in `README.md`; you can run it like this:

```sh
markdown-toc --bullets="-" -i README.md
```

Another lint uses [svg-term](https://github.com/marionebl/svg-term-cli) to convert `example.cast` (generated via [asciinema](https://asciinema.org/)) to `example.svg`; you can run it like this:

```sh
svg-term --window --in example.cast --out example.svg
```
