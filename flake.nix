{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskellPackages;
          hakyllDirectory = ./hakyll;
          packageName = with builtins;
            let cabalFileName = head ((filter (pkgs.lib.hasSuffix ".cabal")) (attrNames (readDir hakyllDirectory)));
            in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${hakyllDirectory}\/${cabalFileName}"));
      in
        {
          packages = {
            ${packageName} = haskellPackages.callCabal2nix packageName hakyllDirectory {};
            default = self.packages.${system}.${packageName};
          };


          devShell = haskellPackages.shellFor {

            # The packages that the shell is for.
            packages = hp:[ self.packages.${system}.default ];

            # Other useful tools
            buildInputs = with haskellPackages;
              [ # haskell-language-server
                cabal-install
              ];

            # Add build inputs of the following derivations.
            inputsFrom = [ ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
