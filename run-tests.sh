#!/usr/bin/env bash

set -eu

nix-shell --pure nix/development.nix --run "runhaskell -Wall -isrc -itest test/Spec.hs"
