{ mkDerivation, accelerate, array, base, bmp, bytestring, fetchgit
, hedgehog, primitive, repa, stdenv, tasty, tasty-hedgehog, vector
}:
mkDerivation {
  pname = "accelerate-io";
  version = "1.2.0.0";
  src = fetchgit {
    url = "https://github.com/AccelerateHS/accelerate-io";
    sha256 = "1pwls4bwm99ylxaswhjrynqi8jwc67d7rmq37g25msmmax43q0g0";
    rev = "c8d281c6e3bcada173535817314330e792b540f0";
  };
  libraryHaskellDepends = [
    accelerate array base bmp bytestring primitive repa vector
  ];
  testHaskellDepends = [
    accelerate array base hedgehog tasty tasty-hedgehog vector
  ];
  homepage = "https://github.com/AccelerateHS/accelerate-io";
  description = "Read and write Accelerate arrays in various formats";
  license = stdenv.lib.licenses.bsd3;
}
