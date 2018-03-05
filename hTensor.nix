{ mkDerivation, fetchgit, base, containers, hmatrix, random, stdenv }:
mkDerivation {
  pname = "hTensor";
  version = "0.9.1patched";
  src = fetchgit {
    url             = "https://github.com/albertoruiz/hTensor.git";
    rev             = "cb97b02941d0e2c5e864afefe3f6313e46d4a9fa";
    sha256          = "0i77fd7a3qb0fg0k19zxj25lbk90hdlhsldf36zfx5qmzryp8n6g";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base containers hmatrix random ];
  #  See also <https://github.com/albertoruiz/hTensor/commit/c11a584104bd0050b0815b9d3139bd597ecbeeaa>.
  preConfigure = ''
    sed -i lib/Numeric/LinearAlgebra/Array/Internal.hs -e "69s/tr x/tr' x/"
  '';
  homepage = "http://perception.inf.um.es/tensor";
  description = "Multidimensional arrays and simple tensor computations";
  license = stdenv.lib.licenses.bsd3;
}
