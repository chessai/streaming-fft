{ mkDerivation, accelerate, accelerate-llvm, base, bytestring, c2hs
, Cabal, cereal, containers, deepseq, directory, dlist, fetchgit
, filepath, ghc, ghc-prim, hashable, libffi, llvm-hs, llvm-hs-pure
, lockfree-queue, mtl, stdenv, template-haskell, unique, unix
, vector
}:
mkDerivation {
  pname = "accelerate-llvm-native";
  version = "1.3.0.0";
  src = fetchgit {
    url = "https://github.com/AccelerateHS/accelerate-llvm";
    sha256 = "0kys38nvd2kk0d3pj807jzrw893g0brhw193xsk3ifhv1zsmii1w";
    rev = "a494652fd371fd30f47501858da1b7c9c3967bf1";
  };
  postUnpack = "sourceRoot+=/accelerate-llvm-native; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    accelerate accelerate-llvm base bytestring Cabal cereal containers
    deepseq directory dlist filepath ghc ghc-prim hashable libffi
    llvm-hs llvm-hs-pure lockfree-queue mtl template-haskell unique
    unix vector
  ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [ accelerate base ];
  description = "Accelerate backend for multicore CPUs";
  license = stdenv.lib.licenses.bsd3;
}
