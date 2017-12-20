{ mkDerivation, base, hTensor, MonadRandom, QuickCheck, stdenv
, template-haskell, vector
}:
mkDerivation {
  pname = "haquil";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [ base hTensor MonadRandom vector ];
  testHaskellDepends = [
    base hTensor MonadRandom QuickCheck template-haskell vector
  ];
  homepage = "https://bitbucket.org/functionally/haquil";
  description = "A Haskell implementation of the Quil instruction set for quantum computing";
  license = stdenv.lib.licenses.mit;
}
