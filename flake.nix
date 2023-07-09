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
          haskellPackages = pkgs.haskellPackages.override {
            overrides = final: prev: {
              hakyll = final.callCabal2nix "hakyll" patchedHakyll {} ;
            };
          };
          patchedHakyll = pkgs.fetchFromGitHub {
            owner = "jwiegley";
            repo = "hakyll";
            rev = "da45c36dfdcf37f90f5e0a02e0ef7d5baeb43c95";
            hash = "sha256-qYS7kWPyebanGlIDEIf48jP/Mc6nGVss7E4+C1VyG8U=";
          };
          hakyllDirectory = ./hakyll;
          packageName = with builtins;
            let cabalFileName = head ((filter (pkgs.lib.hasSuffix ".cabal")) (attrNames (readDir hakyllDirectory)));
            in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${hakyllDirectory}\/${cabalFileName}"));
          hakyll = haskellPackages.callCabal2nix packageName hakyllDirectory {};
      in
        {
          packages = {
            hakyll = pkgs.haskell.lib.overrideCabal hakyll (old: {
              buildDepends = [ pkgs.makeWrapper ];
              postInstall = ''
                            wrapProgram $out/bin/hakyll \
                            --prefix PATH : ${pkgs.lib.getBin pkgs.minify}/bin
                          '';
            });
            default = self.packages.${system}.hakyll;
          };


          devShell = haskellPackages.shellFor {

            # The packages that the shell is for.
            packages = hp: [ self.packages.${system}.hakyll ];

            # Other useful tools
            buildInputs = with haskellPackages;
              [ cabal-install
              ];

            # Add build inputs of the following derivations.
            inputsFrom = [ ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
