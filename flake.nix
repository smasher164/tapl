{
  description = "TAPL";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let supportedSystems = [
      "x86_64-darwin"
      "x86_64-linux"
    ]; in flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import nixpkgs { inherit system; }; in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.go pkgs.mlton pkgs.cargo pkgs.rakudo ];
        };
      });
}
