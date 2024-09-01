{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
            rev = "d13703e1aec0a0d2648a1bf98c80890833ff089f";
            hash = "sha256-BRnBrJKA4gwD9TVp3IDawfX3Er8ZZXSfHFR2Kpx3OME=";
          };
          hakyllDirectory = ./hakyll;
          packageName = with builtins;
            let cabalFileName = head ((filter (pkgs.lib.hasSuffix ".cabal")) (attrNames (readDir hakyllDirectory)));
            in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${hakyllDirectory}\/${cabalFileName}"));
          hakyll = haskellPackages.callCabal2nix packageName hakyllDirectory {};
          latex = pkgs.texlive.combine {
          # Put the packages that we want texlive to use when compiling the PDF in here.
          inherit (pkgs.texlive)
            # scheme-minimal
            # scheme-basic
            # scheme-small
            # scheme-medium
            scheme-full
            latex-bin
            fontspec
            latexmk;
        };
      in
        {
          packages = {
            hakyll = pkgs.haskell.lib.overrideCabal hakyll (old: {
              buildDepends = [ pkgs.makeWrapper ];
              postInstall = ''
                            wrapProgram $out/bin/hakyll \
                            --prefix PATH : ${pkgs.lib.getBin pkgs.minify}/bin \
                            --prefix PATH : ${pkgs.lib.getBin latex}/bin
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
                haskell-language-server
                pkgs.minify
              ];

            # Add build inputs of the following derivations.
            inputsFrom = [ ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
