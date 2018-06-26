{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  accelerate = dontCheck (dontHaddock (doJailbreak (self.callPackage ./deps/accelerate.nix {})));
  accelerate-fft = dontCheck (dontHaddock (doJailbreak (self.callPackage ./deps/accelerate-fft.nix {})));
  accelerate-llvm = dontCheck (self.callPackage ./deps/accelerate-llvm.nix {});
  accelerate-llvm-native = dontCheck (self.callPackage ./deps/accelerate-llvm-native.nix {});
  accelerate-llvm-ptx = dontCheck (self.callPackage ./deps/accelerate-llvm-ptx.nix {});
  accelerate-io = dontCheck (self.callPackage ./deps/accelerate-io.nix {});
  lens-accelerate = dontCheck (self.callPackage ./deps/lens-accelerate.nix {});
  
  cufft = dontCheck (dontHaddock (doJailbreak (self.callPackage ./deps/cufft.nix {})));
  llvm-hs-pure = doJailbreak (dontHaddock (dontCheck (self.callPackage ./deps/llvm-hs-pure.nix {})));
  llvm-hs = self.callHackage "llvm-hs" "6.2.0" { llvm-config = pkgs.llvm_6; };

  primitive = dontBenchmark (dontHaddock (dontCheck (self.callPackage ./deps/primitive.nix {})));
  primitive-indexed = dontCheck (self.callPackage ./deps/primitive-indexed.nix {});
  primitive-checked = doJailbreak (self.callPackage ./deps/primitive-checked.nix {});

  streaming-fft = (
    with rec {
      streaming-fftSource = pkgs.lib.cleanSource ../.;
      streaming-fftBasic = self.callCabal2nix "streaming-fft" streaming-fftSource {};
    };
    overrideCabal streaming-fftBasic (old: {
    })
  );
}
