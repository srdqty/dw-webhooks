#!/usr/bin/env bash

set -eu

cabalArgs="${@}"

nix-shell --pure nix/development.nix --run "cabal ${cabalArgs}"
