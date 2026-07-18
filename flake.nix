{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      rust-overlay,
    }:
    let
      env = pkgs: {
        GIT_BIN = "${pkgs.git}/bin/git";
        NIX_BIN = "${pkgs.nix}/bin/nix";
      };
      overlay =
        final: prev:
        let
          craneLib = crane.mkLib final;
          commonArgs = {
            src = craneLib.cleanCargoSource ./.;
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
        checks = {
          build = pkgs.npc;
          markdown-toc = pkgs.runCommand "npc-markdown-toc" { buildInputs = [ pkgs.markdown-toc ]; } ''
            cp --no-preserve=mode ${./README.md} README.md
            markdown-toc --bullets="-" -i README.md
            diff ${./README.md} README.md
            touch $out
          '';
          svg-term = pkgs.runCommand "npc-svg-term" { buildInputs = [ pkgs.svg-term ]; } ''
            svg-term --window --in ${./example.cast} --out example.svg
            diff ${./example.svg} example.svg
            touch $out
          '';
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.markdown-toc
            pkgs.rust-bin.stable.latest.default
            pkgs.svg-term
          ];
          inherit (env pkgs) GIT_BIN NIX_BIN;
        };
      }
    );
}
