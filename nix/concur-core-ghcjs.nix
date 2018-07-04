{ mkDerivation, base, fetchgit, free, mtl, natural-transformation
, stdenv, stm, transformers
}:
mkDerivation {
  pname = "concur-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ajnsit/concur";
    sha256 = "16zjgqdcss5693q0wn842k9xjx4xdxz9hhizsziv8iy62b9fnwhh";
    rev = "cf08c7dd875033f3d16ed9be3247e626eaad8514";
  };
  postUnpack = "sourceRoot+=/concur-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base free mtl natural-transformation stm transformers
  ];
  homepage = "https://github.com/ajnsit/concur#readme";
  description = "A client side web UI framework for Haskell. Core framework.";
  license = stdenv.lib.licenses.bsd3;
}
