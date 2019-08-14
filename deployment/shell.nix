{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell { buildInputs = [ (pkgs.terraform.withPlugins (p: [ p.aws ])) ]; }
