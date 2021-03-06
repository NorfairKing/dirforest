final: previous:
with final.haskell.lib;

{
  dirforestPackages =
    {
      dirforest =
        doHaddock (
          failOnAllWarnings (
            final.haskellPackages.callCabal2nix "dirforest" (final.gitignoreSource ../dirforest) {}
          )
        );
      genvalidity-dirforest =
        doBenchmark (
          failOnAllWarnings (
            final.haskellPackages.callCabal2nix "genvalidity-dirforest" (final.gitignoreSource ../genvalidity-dirforest) {}
          )
        );
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super: final.dirforestPackages
            );
        }
    );
}
