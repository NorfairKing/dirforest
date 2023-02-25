{ mkDerivation, base, bytestring, containers, criterion, dirforest
, filepath, genvalidity, genvalidity-bytestring
, genvalidity-containers, genvalidity-criterion, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, lib, path, path-io, QuickCheck, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "genvalidity-dirforest";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers dirforest filepath genvalidity
    genvalidity-containers genvalidity-path path QuickCheck
  ];
  testHaskellDepends = [
    base bytestring containers dirforest filepath
    genvalidity-bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson path path-io QuickCheck sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion dirforest genvalidity genvalidity-criterion
    genvalidity-text
  ];
  homepage = "https://github.com/NorfairKing/dirforest#readme";
  description = "Generators for typed directory forests";
  license = lib.licenses.mit;
}
