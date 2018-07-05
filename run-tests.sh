#!/usr/bin/env bash

set -eu

nix-shell --pure nix/development.nix --run "runhaskell -isrc -itest test/Spec.hs"
