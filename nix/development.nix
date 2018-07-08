{ compiler ? import ./ghc.nix }:

let
  pkgs = import ./nixpkgs-pinned {};

  project-root = import ./project-root.nix;

  hlib = pkgs.haskell.lib;

  withTestAndBench = drv: hlib.doCheck (hlib.doBenchmark drv);

  release = import ./release.nix { inherit compiler; };

  development = withTestAndBench release;
in
  development.env.overrideAttrs (old: rec {
    name = compiler + "-" + old.name;

    buildInputs = [
      pkgs.ncurses # Needed by the bash-prompt.sh script
      pkgs.haskellPackages.cabal-install
      pkgs.dhall
    ];

    DW_WEBHOOKS_PROJECT_ROOT=project-root;
    DW_WEBHOOKS_CONFIG_FILE="${project-root}/config.dhall";

    shellHook = old.shellHook + builtins.readFile ./bash-prompt.sh + ''
      source ${pkgs.git.out}/etc/bash_completion.d/git-prompt.sh
      source ${pkgs.git.out}/etc/bash_completion.d/git-completion.bash
    '';
  })
