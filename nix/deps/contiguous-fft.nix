{ mkDerivation, base, contiguous, fetchgit, stdenv }:
mkDerivation {
  pname = "contiguous-fft";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/contiguous-fft.git";
    sha256 = "1jlp23gy05capjv65sg6rabsy1r3db4pdbgdgyvylqsci17sm9dx";
    rev = "f8e5c352add023236bc605f73f4684d9fd58459d";
  };
  libraryHaskellDepends = [ base contiguous ];
  homepage = "https://github.com/chessai/contiguous-fft";
  description = "dft of contiguous memory structures";
  license = stdenv.lib.licenses.bsd3;
}
