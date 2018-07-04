{ mkDerivation, base, colour, containers, h3-core, palette, stdenv
}:
mkDerivation {
  pname = "h3-colour";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base colour containers h3-core palette ];
  homepage = "https://github.com/j-mueller/h3";
  description = "h3 scales that map to colours";
  license = stdenv.lib.licenses.mit;
}
