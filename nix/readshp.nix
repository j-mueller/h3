{ mkDerivation, base, binary, bytestring, data-binary-ieee754
, fetchgit, filepath, monad-loops, stdenv
}:
mkDerivation {
  pname = "readshp";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/j-mueller/readshp";
    sha256 = "0wsn2zdkvymywcq17nk09h6w3imxvzbaz8qs5q6gsw9g0ykcja98";
    rev = "6a09e93e584db96b60b62d560a503dad7f512e29";
  };
  libraryHaskellDepends = [
    base binary bytestring data-binary-ieee754 filepath monad-loops
  ];
  description = "Code for reading ESRI Shapefiles";
  license = stdenv.lib.licenses.mit;
}
