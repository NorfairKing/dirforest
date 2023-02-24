{ mkDerivation, aeson, autodocodec, base, containers, deepseq
, filepath, lib, path, path-io, validity, validity-containers
, validity-path
}:
mkDerivation {
  pname = "dirforest";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base containers deepseq filepath path path-io
    validity validity-containers validity-path
  ];
  homepage = "https://github.com/NorfairKing/dirforest#readme";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
