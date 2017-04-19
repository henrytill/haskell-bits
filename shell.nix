{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghcEnv = pkgs.haskellPackages.ghcWithHoogle (ps: with ps; [
             doctest
             lens
             mtl
             random
           ]);
in
pkgs.stdenv.mkDerivation {
  name = "playground";
  buildInputs = [ ghcEnv ];
  shellHook = "eval $(egrep ^export ${ghcEnv}/bin/ghc)";
}
