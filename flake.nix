{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      system = "x86_64-linux";

      pkgs = nixpkgs.legacyPackages.${system};

      hakyll = pkgs.callPackage ./nix/hakyll.nix {
        src = self;
        myLatex = myLatex;
      };

      myLatex = pkgs.callPackage ./nix/myLatex.nix { };
    in
    {
      packages.${system} = {
        default = hakyll;
      };

      devShells.${system} = {
        default = pkgs.haskellPackages.shellFor {
          # The packages that the shell is for.
          packages = hp: [ hakyll ];

          # Other useful tools
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            pkgs.minify
            myLatex
          ];

          # Add build inputs of the following derivations.
          inputsFrom = [ ];

          # Enables Hoogle for the builtin packages.
          withHoogle = true;
        };
      };
    };
}

# }
