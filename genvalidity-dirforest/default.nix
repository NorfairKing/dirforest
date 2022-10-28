{ mkDerivation, base, bytestring, containers, criterion, dirforest
, filepath, genvalidity, genvalidity-bytestring
, genvalidity-containers, genvalidity-criterion, genvalidity-hspec
, genvalidity-hspec-aeson, genvalidity-path, genvalidity-text
, hspec, lib, path, path-io, pretty-show, QuickCheck
}:
mkDerivation {
  pname = "genvalidity-dirforest";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers dirforest filepath genvalidity
    genvalidity-containers genvalidity-path path QuickCheck
  ];
  testHaskellDepends = [
    base bytestring containers dirforest filepath
    genvalidity-bytestring genvalidity-hspec genvalidity-hspec-aeson
    hspec path path-io pretty-show QuickCheck
  ];
  benchmarkHaskellDepends = [
    base criterion dirforest genvalidity genvalidity-criterion
    genvalidity-text
  ];
  homepage = "https://github.com/NorfairKing/dirforest#readme";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
