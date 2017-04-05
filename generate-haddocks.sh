#! /usr/bin/env sh

nix-shell --command "haddock --hyperlinked-source -h -o doc $*"
