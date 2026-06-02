# Pre-generated to avoid IFD (Import From Derivation) — see the note in the
# top-level flake.nix. Regenerate after changing xbs-hs.cabal with:
#   nix run nixpkgs#cabal2nix -- ./xbs-hs > xbs-hs/default.nix
{ mkDerivation, base, containers, d3x, ihp-hsx, lens, lib, linear
, text, vector, vector-algorithms
}:
mkDerivation {
  pname = "xbs-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers d3x ihp-hsx lens linear text vector
    vector-algorithms
  ];
  doHaddock = false;
  description = "Ball-and-stick molecular viewer rendered as SVG via d3x";
  license = lib.licenses.mit;
}
