{
  description = "bridge-lang";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShells.default =
          pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.cargo
              pkgs.rust-analyzer
              pkgs.rustc
              pkgs.cargo-insta
            ];
          };
      });
}
