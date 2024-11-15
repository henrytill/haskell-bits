{
  description = "A minimal password manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      makeBits =
        system:
        {
          compiler ? "ghc948",
          doCheck ? true,
        }:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          flags = "";
          src = builtins.path {
            path = ./.;
            name = "bits-src";
          };
          bits_ = call compiler "bits" src flags { };
        in
        pkgs.haskell.lib.overrideCabal bits_ (_: {
          inherit doCheck;
        });
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        bits = makeBits system;
      in
      {
        packages.bits = bits { };
        packages.default = self.packages.${system}.bits;
      }
    );
}
