{ project-name }:

with import ../nixpkgs-pinned {};

stdenv.mkDerivation rec {
  name = "change-project-name";

  project-root = import ../project-root.nix;

  buildInputs = [
    haskellPackages.hpack
    cabal2nix
  ];

  shellHook = ''
    set -eu

    paths="app
    ChangeLog.md
    nix
    package.yaml
    README.md
    src
    test
    "

    for path in $paths; do
      echo "Update ${project-root}/$path"
      find "${project-root}/$path" \
        -type f \
        ! -name "change-project-name.nix" \
        | xargs -r sed -i -e "s/haskell-project-template/${project-name}/g"
    done

    rm -f ${project-root}/haskell-project-template.cabal

    nix-shell --pure ${project-root}/nix/scripts/generate-cabal-and-nix-file.nix

    exit
  '';
}
