{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ ghc cabal-install haskellPackages.happy pkg-config graphviz haskellPackages.haskell-language-server ];
}
