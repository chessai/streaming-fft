{ mkDerivation, accelerate, base, bytestring, containers
, data-default-class, deepseq, directory, dlist, exceptions
, fetchgit, filepath, llvm-hs, llvm-hs-pure, mtl, stdenv
, template-haskell, unordered-containers, vector
}:
mkDerivation {
  pname = "accelerate-llvm";
  version = "1.3.0.0";
  src = fetchgit {
    url = "https://github.com/AccelerateHS/accelerate-llvm";
    sha256 = "0kys38nvd2kk0d3pj807jzrw893g0brhw193xsk3ifhv1zsmii1w";
    rev = "a494652fd371fd30f47501858da1b7c9c3967bf1";
  };
  postUnpack = "sourceRoot+=/accelerate-llvm; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    accelerate base bytestring containers data-default-class deepseq
    directory dlist exceptions filepath llvm-hs llvm-hs-pure mtl
    template-haskell unordered-containers vector
  ];
  description = "Accelerate backend component generating LLVM IR";
  license = stdenv.lib.licenses.bsd3;
}
