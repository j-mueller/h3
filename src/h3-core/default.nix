{ mkDerivation, base, containers, format-numbers, profunctors
, semigroupoids, semigroups, stdenv, text
}:
mkDerivation {
  pname = "h3-core";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers format-numbers profunctors semigroupoids semigroups
    text
  ];
  homepage = "https://github.com/j-mueller/h3";
  description = "Core library for h3";
  license = stdenv.lib.licenses.mit;
}
