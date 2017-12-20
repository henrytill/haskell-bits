{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  ghcEnv      = pkgs.haskellPackages.ghcWithHoogle (import ./packages.nix);
  dockerImage = pkgs.dockerTools.buildImage {
                  name = "hoogle";
                  contents = [ ghcEnv ];
                  config = {
                    Cmd = [ "hoogle" "server" "--local" "--host=*" "-p" "80" ];
                    ExposedPorts = {
                      "80/tcp" = {};
                    };
                  };
                }
in
  dockerImage
