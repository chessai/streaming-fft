{ mkDerivation, base, contiguous, fetchgit, stdenv }:
mkDerivation {
  pname = "contiguous-fft";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/contiguous-fft.git";
    sha256 = "0syc3ncijfxvlhwl28bi5gi9p4lrnxii1pf330i6yin26z3nc3hh";
    rev = "a1dfaebea8682e72aeb1d8b0c325779dbe2e46fc";
  };
  libraryHaskellDepends = [ base contiguous ];
  homepage = "https://github.com/chessai/contiguous-fft";
  description = "dft of contiguous memory structures";
  license = stdenv.lib.licenses.bsd3;
}
