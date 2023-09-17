let
  config = {
    packageOverrides = pkgs: {
      haskell-language-server = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "96" ];
      };
    };
  };

in
  {
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e7f38be3775bab9659575f192ece011c033655f0.tar.gz") { inherit config; }
  }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.cabal-install
      pkgs.gcc
      pkgs.gdb
      pkgs.haskell.compiler.ghc96
      pkgs.haskell-language-server
      pkgs.haskellPackages.hoogle
      pkgs.nasm
    ];
  }
