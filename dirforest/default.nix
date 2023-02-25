{ mkDerivation, aeson, autodocodec, base, containers, deepseq
, filepath, lib, path, path-io, validity, validity-containers
, validity-path
}:
mkDerivation {
  pname = "dirforest";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base containers deepseq filepath path path-io
    validity validity-containers validity-path
  ];
  homepage = "https://github.com/NorfairKing/dirforest#readme";
  description = "Typed directory forest";
  license = lib.licenses.mit;
}
