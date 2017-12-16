{ mkDerivation, base, hTensor, QuickCheck, stdenv }:
mkDerivation {
  pname = "quil";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hTensor ];
  testHaskellDepends = [ base hTensor QuickCheck ];
  homepage = "https://bitbucket.org/functionally/quil";
  description = "A Haskell implementation of the Quil instruction set for quantum computing";
  license = stdenv.lib.licenses.mit;
}
