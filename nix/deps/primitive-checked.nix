{ mkDerivation, base, fetchgit, primitive, stdenv }:
mkDerivation {
  pname = "primitive-checked";
  version = "0.6.4.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-checked.git";
    sha256 = "0f0ynwzp2cmkx6q24y486kkq83l6ix0flz9w44v142w4cbz19cy1";
    rev = "42dc55615c7251975d7fef65617560d9403448be";
  };
  libraryHaskellDepends = [ base primitive ];
  homepage = "https://github.com/andrewthad/primitive-checked#readme";
  description = "primitive functions with bounds-checking";
  license = stdenv.lib.licenses.bsd3;
}
