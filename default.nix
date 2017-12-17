{ mkDerivation, base, hTensor, QuickCheck, stdenv, template-haskell
, vector
}:
mkDerivation {
  pname = "quil";
  version = "0.1.4.0";
  src = ./.;
  libraryHaskellDepends = [ base hTensor vector ];
  testHaskellDepends = [
    base hTensor QuickCheck template-haskell vector
  ];
  homepage = "https://bitbucket.org/functionally/quil";
  description = "A Haskell implementation of the Quil instruction set for quantum computing";
  license = stdenv.lib.licenses.mit;
}
