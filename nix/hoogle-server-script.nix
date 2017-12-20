{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  ghcEnv = pkgs.haskellPackages.ghcWithHoogle (import ./packages.nix);
  script = pkgs.writeScriptBin "hoogle-server" ''
             #! ${pkgs.stdenv.shell}
             ${ghcEnv}/bin/hoogle server --local -p ''${1:-8080}
           '';
in
  script
