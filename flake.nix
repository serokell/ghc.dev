{
  description = "ghc-dev";

  inputs = {
    terranix = {
      url = "github:terranix/terranix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    serokell-nix = {
      url = "github:serokell/serokell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    { self, nixpkgs, serokell-nix, terranix }:
    let
      system = "x86_64-linux";
      ghc = "ghc92";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          ghc-dev-webgen = haskellPackages.callCabal2nix "ghc-dev-webgen" "${self}/src/" {};
        });
      tfConfigAst = terranix.lib.terranixConfigurationAst {
        inherit system pkgs;
        modules = [ ./deployment/main.nix ];
      };
      tfLib = serokell-nix.lib.terraform { inherit pkgs tfConfigAst; };
    in
    {
      packages.${system} = {
        ghc-dev-webgen = haskellPackages.ghc-dev-webgen;
        ghc-dev =
          pkgs.stdenv.mkDerivation {
            name = "ghc-dev";
            buildCommand = ''
              mkdir -p "$out/out"
              "${haskellPackages.ghc-dev-webgen}"/bin/ghc-dev-webgen "$out/out"
            '';
          };

      };
      defaultPackage.${system} = self.packages.${system}.ghc-dev;
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages(p: p.ghc-dev-webgen.getCabalDeps.executableHaskellDepends))
          haskellPackages.ghc-dev-webgen.getCabalDeps.executableToolDepends
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
      };

      # nix run .#tf-plan
      # nix run .#tf-apply
      apps.${system} = tfLib.mkApps [ "plan" "apply" ];
    };
}
