final:
previous:
with final.haskell.lib;
{
  dirforestPackages =
    let dirforestPkg = name:
      (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "dirforest"
      "genvalidity-dirforest"
    ]
      dirforestPkg;
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: final.dirforestPackages
    );
  });
}
