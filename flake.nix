{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # This is the last stable release with a Git version earlier than v2.48.0,
    # which introduced a bug that sometimes causes `git fetch` to fail on
    # partial clones like the one `npc` uses.
    nixpkgs-git.url = "github:NixOS/nixpkgs/nixos-24.11";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      rust-overlay,
      nixpkgs-git,
    }:
    let
      env = pkgs: {
        GIT_BIN = "${(import nixpkgs-git { system = pkgs.stdenv.hostPlatform.system; }).git}/bin/git";
        NIX_BIN = "${pkgs.nix}/bin/nix";
      };
      overlay =
        final: prev:
        let
          craneLib = crane.mkLib final;
          commonArgs = {
            src = ./.;
            strictDeps = true;
          };
        in
        {
          npc = craneLib.buildPackage (
            commonArgs
            // {
              cargoArtifacts = craneLib.buildDepsOnly commonArgs;
              inherit (env final) GIT_BIN NIX_BIN;
            }
          );
        };
    in
    {
      overlays.default = overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import rust-overlay)
            overlay
          ];
        };
      in
      {
        packages.default = pkgs.npc;
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.rust-bin.stable.latest.default
          ];
          inherit (env pkgs) GIT_BIN NIX_BIN;
        };
      }
    );
}
