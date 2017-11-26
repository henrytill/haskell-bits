{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;

  packages = ps: with ps;
    [ HUnit
      QuickCheck
      attoparsec
      async
      bifunctors
      bytestring
      categories
      comonad
      containers
      criterion
      dlist
      doctest
      free
      hashable
      kan-extensions
      lens
      lens-family
      mtl
      operational
      optparse-applicative
      parsec
      process
      process-extras
      random
      recursion-schemes
      smallcheck
      tasty
      tasty-hunit
      tasty-quickcheck
      text
      transformers
      unix
      unix-compat
      vector
      wai
      warp
      zlib
    ];

  ghcEnv = pkgs.haskellPackages.ghcWithHoogle packages;

in pkgs.stdenv.mkDerivation {
  name = "playground";
  buildInputs = [ ghcEnv ];
  shellHook = "eval $(egrep ^export ${ghcEnv}/bin/ghc)";
}
