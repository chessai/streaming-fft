{ mkDerivation, base, doctest, fetchgit, primitive, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "primitive-indexed";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-indexed.git";
    sha256 = "0yl53pvzcm1g6q4sfaqw5gybvyv1qqp2c58xs3jb56ayry23g7fg";
    rev = "fe8aa881e5c224453b51db23c7a31632f8688fbd";
  };
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [ base doctest QuickCheck ];
  homepage = "https://github.com/andrewthad/primitive-indexed#readme";
  license = stdenv.lib.licenses.bsd3;
}
