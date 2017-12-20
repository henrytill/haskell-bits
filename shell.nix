{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  ghcEnv = pkgs.haskellPackages.ghcWithHoogle (import ./nix/packages.nix);
  bits   = pkgs.stdenv.mkDerivation {
             name = "bits";
             buildInputs = [ ghcEnv ];
             shellHook = "eval $(egrep ^export ${ghcEnv}/bin/ghc)";
           };
in
  bits
