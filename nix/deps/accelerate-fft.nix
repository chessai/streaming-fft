{ mkDerivation, accelerate, accelerate-llvm, accelerate-llvm-native
, accelerate-llvm-ptx, base, bytestring, carray, containers, cuda
, cufft, fetchgit, fft, file-embed, hashable, hedgehog
, lens-accelerate, mtl, stdenv, tasty, tasty-hedgehog
, unordered-containers
}:
mkDerivation {
  pname = "accelerate-fft";
  version = "1.2.0.0";
  src = fetchgit {
    url = "https://github.com/AccelerateHS/accelerate-fft.git";
    sha256 = "1bm1fi9xk2zbcb8y86rns0r7zk2lvr473xz0q3mjvgi0cq950g8v";
    rev = "19e7674b454a3c4b429f47e10c0a5ba5d128b11f";
  };
  libraryHaskellDepends = [
    accelerate accelerate-llvm accelerate-llvm-native
    accelerate-llvm-ptx base bytestring carray containers cuda cufft
    fft file-embed hashable lens-accelerate mtl unordered-containers
  ];
  testHaskellDepends = [
    accelerate accelerate-llvm-native accelerate-llvm-ptx base hedgehog
    tasty tasty-hedgehog
  ];
  homepage = "https://github.com/AccelerateHS/accelerate-fft";
  description = "FFT using the Accelerate library";
  license = stdenv.lib.licenses.bsd3;
}
