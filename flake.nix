{
  description = "TAPL";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.nixpkgs-raku.url = "github:nixos/nixpkgs/c4b95abcb3664bc5378f0b9cebbc0fa0bf508881";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-raku, flake-utils }:
    let supportedSystems = [
      "x86_64-darwin"
      "x86_64-linux"
    ]; in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs-raku = import nixpkgs-raku { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.go pkgs.mlton pkgs.cargo pkgs-raku.rakudo ];
        };
      });
}
