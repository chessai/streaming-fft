{ compiler ? "ghc843" }:

with rec {
  pkgs = (import ./nix/nixpkgs.nix {
    inherit compiler; 
  });
  drv = pkgs.haskellPackages.streaming-fft;
};

drv
