{ mkDerivation, base, concur-core, fetchgit, free, ghcjs-base-stub
, mtl, stdenv, stm, transformers
}:
mkDerivation {
  pname = "concur-react";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/j-mueller/concur";
    sha256 = "0z9a6890a0cjh6kynv7b8a40bj275ks0m2zxm8r93viia806lsrk";
    rev = "71ea66eb338425965edfdd65eb10c5a1fb833149";
  };
  postUnpack = "sourceRoot+=/concur-react; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base concur-core free ghcjs-base-stub mtl stm transformers
  ];
  homepage = "https://github.com/ajnsit/concur#readme";
  description = "A client side web UI framework for Haskell. React bindings.";
  license = stdenv.lib.licenses.bsd3;
}
