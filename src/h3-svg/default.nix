{ mkDerivation, base, h3-core, stdenv, svg-builder, text }:
mkDerivation {
  pname = "h3-svg";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base h3-core svg-builder text ];
  homepage = "https://github.com/j-mueller/h3";
  description = "svg-builder bindings for h3";
  license = stdenv.lib.licenses.mit;
}
