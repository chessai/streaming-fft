{ mkDerivation, base, fetchgit, primitive, stdenv }:
mkDerivation {
  pname = "contiguous";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/contiguous.git";
    sha256 = "04pqn5w0v8cxwbblm71jmh1zgvq3f3q51fc221v8jldhmphzyhd3";
    rev = "40751a45afead764b248c72f894e4f0e93cbe77d";
  };
  libraryHaskellDepends = [ base primitive ];
  homepage = "https://github.com/andrewthad/contiguous";
  description = "Unified interface for primitive arrays";
  license = stdenv.lib.licenses.bsd3;
}
