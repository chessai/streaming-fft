{ mkDerivation, base, containers, deepseq, fetchgit, gauge
, ghc-prim, primitive, QuickCheck, stdenv, tasty, tasty-quickcheck
, weigh
}:
mkDerivation {
  pname = "primitive-median";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-median.git";
    sha256 = "1qac7glxx3kx86jk4dlgzlfrvj7bksd8vyf1sispq9wfpihqqalf";
    rev = "ad374a735082b4000b16d2cbccad1cfb757259ac";
  };
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [
    base containers primitive QuickCheck tasty tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base deepseq gauge ghc-prim primitive weigh
  ];
  homepage = "https://github.com/andrewthad/primitive-median#readme";
  license = stdenv.lib.licenses.bsd3;
}
