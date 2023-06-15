{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ ghc cabal-install haskellPackages.happy pkg-config glib cairo pango haskellPackages.haskell-language-server ];
}
