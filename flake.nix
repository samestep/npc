{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # This is the last `nixpkgs-unstable` commit with a Git version earlier than
    # v2.48.0, which introduced a bug that sometimes causes `git fetch` to fail
    # on partial clones like the one `npc` uses.
    nixpkgs-git-2-47-2.url = "github:NixOS/nixpkgs/dad564433178067be1fbdfcce23b546254b6d641";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      rust-overlay,
      nixpkgs-git-2-47-2,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
        craneLib = crane.mkLib pkgs;
        commonArgs = {
          src = ./.;
          strictDeps = true;
        };
        GIT_BIN = "${(import nixpkgs-git-2-47-2 { inherit system; }).git}/bin/git";
        NIX_BIN = "${pkgs.nix}/bin/nix";
      in
      {
        packages.default = craneLib.buildPackage (
          commonArgs
          // {
            cargoArtifacts = craneLib.buildDepsOnly commonArgs;
            inherit GIT_BIN NIX_BIN;
          }
        );
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.nixfmt
            pkgs.rust-bin.stable.latest.default
          ];
          inherit GIT_BIN NIX_BIN;
        };
      }
    );
}
