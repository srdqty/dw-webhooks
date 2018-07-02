{ compiler ? import ./ghc.nix
, extraHaskellOverride ? (pkgs: new: old: {})
}:
let
  haskellOverlay = (self: super: {
    haskellPackages = super.haskell.packages."${compiler}".override {
      overrides = (new: old: {
        resourcet = old.resourcet_1_1_11;
        conduit = old.conduit_1_2_13_1;
        conduit-extra = old.conduit-extra_1_2_3_2;
        conduit-combinators = old.callHackage "conduit-combinators" "1.1.2" { };
        xml-conduit = old.xml-conduit_1_7_1_2;
        http-conduit = old.http-conduit_2_2_4;
        hpack = old.hpack_0_28_2;
        hnix = old.callHackage "hnix" "0.4.0" {};
        deriving-compat = old.callHackage "deriving-compat" "0.3.6" {};
        dhall = old.callHackage "dhall" "1.14.0" {};
        dhall-json = old.callHackage "dhall-json" "1.2.0" {};
      } // extraHaskellOverride self new old);
    };
  });

  nonHaskellOverlay = (self: super: {
  });
in
  [nonHaskellOverlay haskellOverlay]
