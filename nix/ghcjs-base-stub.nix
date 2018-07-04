{ mkDerivation, aeson, attoparsec, base, containers, deepseq
, fetchgit, ghc-prim, primitive, scientific, stdenv, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "ghcjs-base-stub";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/louispan/ghcjs-base-stub";
    sha256 = "1niqw9cynsmafbxa4jgj8yz2p2kv25n2h0wsp5dd3kjg2kizmn0w";
    rev = "d988e9eb9098a5f56808cbfae29be8b16729e437";
  };
  libraryHaskellDepends = [
    aeson attoparsec base containers deepseq ghc-prim primitive
    scientific text transformers unordered-containers vector
  ];
  homepage = "https://github.com/louispan/javascript-stub#readme";
  description = "Allow GHCJS projects to compile under GHC and develop using intero";
  license = stdenv.lib.licenses.bsd3;
}
