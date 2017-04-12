{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithHoogle (ps: with ps; [
          doctest
          mtl
          random
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "playground";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
