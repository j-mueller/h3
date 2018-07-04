{ mkDerivation, base, concur-core, concur-react, ghcjs-base-stub
, h3-core, stdenv
}:
mkDerivation {
  pname = "h3-concur-react";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base concur-core concur-react ghcjs-base-stub h3-core
  ];
  homepage = "https://github.com/j-mueller/h3";
  description = "concur-react bindings for h3";
  license = stdenv.lib.licenses.mit;
}
