{ mkDerivation, accelerate, base, fetchgit, lens, stdenv }:
mkDerivation {
  pname = "lens-accelerate";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/tmcdonell/lens-accelerate";
    sha256 = "0cnsiqg99axcvf3c39q4qiww2nvwa6qg1qb1a8han7r3zldssypy";
    rev = "6fdc2141d595da1056c59cab77f11594686b7cdd";
  };
  libraryHaskellDepends = [ accelerate base lens ];
  homepage = "https://github.com/tmcdonell/lens-accelerate";
  description = "Instances to mix lens with accelerate";
  license = stdenv.lib.licenses.bsd3;
}
