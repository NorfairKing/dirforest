final:
prev:
with final.lib;
with final.haskell.lib;
{
  dirforestRelease =
    final.symlinkJoin {
      name = "dirforest-release";
      paths = attrValues final.haskellPackages.dirforestPackages;
    };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let dirforestPackages =
          let dirforestPkg = name:
            (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name ((../. + "/${name}")) "--no-hpack" { }));
          in
          final.lib.genAttrs [
            "dirforest"
            "genvalidity-dirforest"
          ]
            dirforestPkg;
        in { inherit dirforestPackages; } // dirforestPackages
    );
  });
}
