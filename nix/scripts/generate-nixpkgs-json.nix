{ owner ? "NixOS"
, repo ? "nixpkgs"
, rev ? "f5b0d6d88963b77659348805f5347bb6655ec713"
}:

let
  pkgs = import <nixpkgs> {};

  url="https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";

  newNix = 0 <= builtins.compareVersions builtins.nixVersion "1.12";

  file = if newNix then
    builtins.fetchTarball { url = url; }
  else
    builtins.fetchurl url;

  json = builtins.toFile "data.json" ''
    { "url": "${url}"
    , "rev": "${rev}"
    , "owner": "${owner}"
    , "repo": "${repo}"
    }
  '';

  out-filename = if
    newNix
  then
    builtins.toString ../nixpkgs-pinned/nixpkgs-2.0.json
  else
    builtins.toString ../nixpkgs-pinned/nixpkgs-1.11.json
  ;

  sha256calc = if newNix then
    "nix-hash --type sha256 --base32 ${file}"
  else
    "sha256sum -b ${file} | cut -d ' ' -f 1";
in


pkgs.stdenv.mkDerivation rec {
  name = "generate-nixpkgs-json";

  buildInputs = [
    pkgs.jq
    pkgs.nix
  ];

  shellHook = ''
    set -eu
    sha256=$(${sha256calc})
    jq .+="{\"sha256\":\"$sha256\"}" ${json} > ${out-filename}
    exit
  '';
}
