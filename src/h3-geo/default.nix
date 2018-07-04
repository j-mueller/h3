{ mkDerivation, base, directory, filepath, h3-core, profunctors
, readshp, semigroupoids, semigroups, stdenv
}:
mkDerivation {
  pname = "h3-geo";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base directory filepath h3-core profunctors readshp semigroupoids
    semigroups
  ];
  homepage = "https://github.com/j-mueller/h3";
  description = "Read shapefiles and project geometric shapes with h3";
  license = stdenv.lib.licenses.mit;
}
