let

  config = import ./config.nix;

  pkgs = import <nixpkgs> { inherit config; };

  ghcEnv = pkgs.haskellPackages.ghcWithHoogle (import ./packages.nix);

  jobs = rec {

    dockerImage = pkgs.dockerTools.buildImage {
      name = "hoogle";
      contents = [ ghcEnv ];
      config = {
        Cmd = [ "hoogle" "server" "--local" "--host=*" "-p" "80" ];
        ExposedPorts = {
          "80/tcp" = {};
        };
      };
    };

    script = pkgs.writeScriptBin "hoogle-server" ''
      #! ${pkgs.stdenv.shell}
      ${ghcEnv}/bin/hoogle server --local -p ''${1:-8080}
    '';

    shell = pkgs.stdenv.mkDerivation {
      name = "bits";
      buildInputs = [ ghcEnv script ];
      shellHook = "eval $(egrep ^export ${ghcEnv}/bin/ghc)";
    };
  };

in

  jobs
