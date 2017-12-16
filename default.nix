{ mkDerivation, base, hTensor, QuickCheck, stdenv, vector }:
mkDerivation {
  pname = "quil";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [ base hTensor vector ];
  testHaskellDepends = [ base hTensor QuickCheck vector ];
  homepage = "https://bitbucket.org/functionally/quil";
  description = "A Haskell implementation of the Quil instruction set for quantum computing";
  license = stdenv.lib.licenses.mit;
}
