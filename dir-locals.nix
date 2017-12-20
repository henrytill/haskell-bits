{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  ghcEnv = pkgs.haskellPackages.ghcWithHoogle (import ./nix/packages.nix);
  bits   = pkgs.nixBufferBuilders.withPackages [ ghcEnv ];
in
  bits
