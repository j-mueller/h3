{ mkDerivation, array, base, colour, containers, fetchgit
, MonadRandom, stdenv
}:
mkDerivation {
  pname = "palette";
  version = "0.3.0.1";
  src = fetchgit {
    url = "https://github.com/diagrams/palette";
    sha256 = "02mlgdvppcpiygkcb9qn2857g4h39kqrd9i8pynnc6i61bqswcgb";
    rev = "780593f9522b3c85cb06f7aa691fa83f88f9ace9";
  };
  libraryHaskellDepends = [
    array base colour containers MonadRandom
  ];
  homepage = "http://projects.haskell.org/diagrams";
  description = "Utilities for choosing and creating color schemes";
  license = stdenv.lib.licenses.bsd3;
}
