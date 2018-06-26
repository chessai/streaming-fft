{ mkDerivation, ansi-terminal, ansi-wl-pprint, base, base-orphans
, bytestring, Cabal, cabal-doctest, constraints, containers
, cryptonite, deepseq, directory, doctest, exceptions, fetchgit
, filepath, ghc-prim, half, hashable, hashtables, hedgehog, lens
, mtl, stdenv, tasty, tasty-expected-failure, tasty-hedgehog
, tasty-hunit, template-haskell, terminal-size, transformers
, unique, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "accelerate";
  version = "1.3.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/accelerate";
    sha256 = "1n49zfabwvyyrq73g58y6b57crzgil7f5h8rizfx3j2sh057vyvg";
    rev = "1088dcd1b33c26bf286aada9318a5184275e2643";
  };
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    ansi-terminal ansi-wl-pprint base base-orphans bytestring
    constraints containers cryptonite deepseq directory exceptions
    filepath ghc-prim half hashable hashtables hedgehog lens mtl tasty
    tasty-expected-failure tasty-hedgehog tasty-hunit template-haskell
    terminal-size transformers unique unix unordered-containers vector
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/AccelerateHS/accelerate/";
  description = "An embedded language for accelerated array processing";
  license = stdenv.lib.licenses.bsd3;
}
