{ mkDerivation, base, Cabal, containers, criterion, deepseq
, fetchgit, fingertree, QuickCheck, random, SegmentTree, stdenv
, weigh
}:
mkDerivation {
  pname = "IntervalMap";
  version = "0.6.0.0";
  src = fetchgit {
    url = "https://github.com/bokesan/IntervalMap";
    sha256 = "0jwkssq3nfkpwwvzjixp9dy6320vv79ypbdf08sxvf6xy8zh4d8q";
    rev = "235074dff3d56cd764592242889a8bb0d420145e";
  };
  libraryHaskellDepends = [ base containers deepseq ];
  testHaskellDepends = [ base Cabal containers deepseq QuickCheck ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq fingertree random SegmentTree
    weigh
  ];
  homepage = "http://www.chr-breitkopf.de/comp/IntervalMap";
  description = "Containers for intervals, with efficient search";
  license = stdenv.lib.licenses.bsd3;
}
