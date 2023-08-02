# let
#   config = {
#     packageOverrides = pkgs: {
#       haskell-language-server = pkgs.haskell-language-server.override {
#         supportedGhcVersions = [ "943" ];
#       };
#     };
#   };

# in
  {
    # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/fb942492b7accdee4e6d17f5447091c65897dde4.tar.gz") { inherit config; }
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/fb942492b7accdee4e6d17f5447091c65897dde4.tar.gz") { }
  }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.cabal-install
      # pkgs.gcc
      # pkgs.gdb
      pkgs.haskell.compiler.ghc96
      pkgs.haskell-language-server
      pkgs.haskellPackages.hoogle
      # pkgs.nasm
    ];
  }
