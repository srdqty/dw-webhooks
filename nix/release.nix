{ compiler ? import ./ghc.nix }:

let
  haskellOverrides = pkgs: new: old: {
    dw-webhooks = old.callPackage ./.. { };
  };

  overlays = import ./overlays.nix {
    extraHaskellOverride = haskellOverrides;
  };

  pkgs = import ./nixpkgs-pinned {
    overlays = overlays;
  };
in
  pkgs.haskellPackages.dw-webhooks
