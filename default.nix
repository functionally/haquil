{ mkDerivation, base, bv, data-binary-ieee754, data-default
, hTensor, MonadRandom, QuickCheck, stdenv, template-haskell
, vector
}:
mkDerivation {
  pname = "haquil";
  version = "0.2.1.4";
  src = ./.;
  libraryHaskellDepends = [
    base bv data-binary-ieee754 data-default hTensor MonadRandom vector
  ];
  testHaskellDepends = [
    base bv data-binary-ieee754 data-default hTensor MonadRandom
    QuickCheck template-haskell vector
  ];
  homepage = "https://bitbucket.org/functionally/haquil";
  description = "A Haskell implementation of the Quil instruction set for quantum computing";
  license = stdenv.lib.licenses.mit;
}
