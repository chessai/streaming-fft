{ mkDerivation, base, c2hs, Cabal, cuda, directory, fetchgit
, filepath, stdenv, template-haskell
}:
mkDerivation {
  pname = "cufft";
  version = "0.9.0.0";
  src = fetchgit {
    url = "https://github.com/robeverest/cufft";
    sha256 = "1124h2q49dp10kq6l0rxjh4940ly5qbpvv8xsm0d3r5pmgxdjl5c";
    rev = "f3b757abbb921345f74439cfde68a6a393a3e170";
  };
  setupHaskellDepends = [
    base Cabal cuda directory filepath template-haskell
  ];
  libraryHaskellDepends = [ base cuda ];
  libraryToolDepends = [ c2hs ];
  homepage = "https://github.com/robeverest/cufft";
  description = "Haskell bindings for the CUFFT library";
  license = stdenv.lib.licenses.bsd3;
}
