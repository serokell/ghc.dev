{
  description = "ghc-dev";

  inputs = {
    terranix.url = "github:terranix/terranix";
    terranix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self, nixpkgs, terranix }:
    let
      system = "x86_64-linux";
      ghc = "ghc92";
      pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          ghc-dev-webgen = haskellPackages.callCabal2nix "ghc-dev-webgen" "${self}/src/" {};
        });
      terraform = pkgs.terraform;
      terraformConfiguration = terranix.lib.terranixConfiguration {
        inherit system;
        modules = [ ./deployment/main.nix ];
      };
      mkTfApp = command: {
          type = "app";
          program = toString (pkgs.writers.writeBash "plan" ''
            if [[ -e config.tf.json ]]; then rm -f config.tf.json; fi
            cp ${terraformConfiguration} config.tf.json \
              && ${terraform}/bin/terraform init \
              && ${terraform}/bin/terraform ${command}
          '');
      };
      mkTfApps = commands: lib.mapAttrs' (name: lib.nameValuePair "tf-${name}") ( lib.genAttrs commands mkTfApp);
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

      devShells.${system}.tf = pkgs.mkShell {
        buildInputs = [
          terraform
        ];
      };

      # nix run .#tf-plan
      # nix run .#tf-apply
      apps.${system} = mkTfApps [ "plan" "apply" ];
    };
}
