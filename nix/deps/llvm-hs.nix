{ mkDerivation, array, attoparsec, base, bytestring, Cabal
, containers, exceptions, fetchgit, llvm-config, llvm-hs-pure, mtl
, pretty-show, process, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, transformers
, utf8-string
}:
mkDerivation {
  pname = "llvm-hs";
  version = "6.2.0";
  src = fetchgit {
    url = "https://github.com/llvm-hs/llvm-hs";
    sha256 = "0h00kaqhnfpqvp6ppz8g9wlyblcjzkgmmr4akrna7s1gmlxi5smw";
    rev = "f4be944abbcaff98718ad8d99e80a3bf506b075f";
  };
  postUnpack = "sourceRoot+=/llvm-hs; echo source root reset to $sourceRoot";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers exceptions llvm-hs-pure
    mtl template-haskell transformers utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base bytestring containers llvm-hs-pure mtl pretty-show process
    QuickCheck tasty tasty-hunit tasty-quickcheck temporary
    transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
