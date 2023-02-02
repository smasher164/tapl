{
  description = "TAPL";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let supportedSystems = [
      "x86_64-darwin"
      "x86_64-linux"
    ]; in flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import nixpkgs { inherit system; }; in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.gopls pkgs.go-outline pkgs.go-tools pkgs.gopkgs pkgs.delve pkgs.go_1_18 pkgs.gotools
            pkgs.rustc pkgs.cargo pkgs.clippy pkgs.rustfmt
            pkgs.mlton pkgs.rakudo
            pkgs.re2c
          ];
        };
      });
}
