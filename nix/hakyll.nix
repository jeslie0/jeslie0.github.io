{
  lib,
  haskellPackages,
  makeWrapper,
  fetchFromGitHub,
  haskell,
  minify,
  myLatex,
  src,
}:

let
  patchedHakyll = fetchFromGitHub {
    owner = "jeslie0";
    repo = "hakyll";
    rev = "01dfbd02fb03cdfa26ea2dd86c9880885df86da2";
    hash = "sha256-me8fLNEQr8hkQfq4Uw3SeckCbcALk2oQJVxg6NPPedg=";
  };

  hPackages = haskellPackages.override {
    overrides = final: prev: {
      hakyll = final.callCabal2nix "hakyll" patchedHakyll { };
    };
  };

  siteBuilder = hPackages.callCabal2nix "site-builder" src { };
in

haskell.lib.overrideCabal siteBuilder (old: {
  buildDepends = [ makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/hakyll \
    --prefix PATH : ${lib.getBin minify}/bin \
    --prefix PATH : ${lib.getBin myLatex}/bin
  '';
})
