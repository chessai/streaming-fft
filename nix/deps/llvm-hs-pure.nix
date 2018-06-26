{ mkDerivation, attoparsec, base, bytestring, containers, fail
, fetchgit, mtl, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, transformers, unordered-containers
}:
mkDerivation {
  pname = "llvm-hs-pure";
  version = "6.2.0";
  src = fetchgit {
    url = "https://github.com/llvm-hs/llvm-hs";
    sha256 = "0h00kaqhnfpqvp6ppz8g9wlyblcjzkgmmr4akrna7s1gmlxi5smw";
    rev = "f4be944abbcaff98718ad8d99e80a3bf506b075f";
  };
  postUnpack = "sourceRoot+=/llvm-hs-pure; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring containers fail mtl template-haskell
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base containers mtl tasty tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}
