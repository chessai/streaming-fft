{ mkDerivation, accelerate, accelerate-llvm, base, bytestring
, containers, cuda, deepseq, directory, dlist, fetchgit, file-embed
, filepath, hashable, llvm-hs, llvm-hs-pure, mtl, nvvm, pretty
, process, stdenv, template-haskell, unordered-containers
}:
mkDerivation {
  pname = "accelerate-llvm-ptx";
  version = "1.3.0.0";
  src = fetchgit {
    url = "https://github.com/AccelerateHS/accelerate-llvm";
    sha256 = "0kys38nvd2kk0d3pj807jzrw893g0brhw193xsk3ifhv1zsmii1w";
    rev = "a494652fd371fd30f47501858da1b7c9c3967bf1";
  };
  postUnpack = "sourceRoot+=/accelerate-llvm-ptx; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    accelerate accelerate-llvm base bytestring containers cuda deepseq
    directory dlist file-embed filepath hashable llvm-hs llvm-hs-pure
    mtl nvvm pretty process template-haskell unordered-containers
  ];
  testHaskellDepends = [ accelerate base ];
  description = "Accelerate backend for NVIDIA GPUs";
  license = stdenv.lib.licenses.bsd3;
}
