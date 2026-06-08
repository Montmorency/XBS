# Pre-generated to avoid IFD (Import From Derivation) — see the note in the
# top-level flake.nix. Regenerate after changing xbs-hs.cabal with:
#   nix run nixpkgs#cabal2nix -- ./xbs-hs > xbs-hs/default.nix
{ mkDerivation, base, bytestring, oleg-delimcc, containers, d3x, filepath
, hspec, http-types, ihp-hsx, lens, lib, linear, stm, text
, uu-parsinglib, vector, vector-algorithms, vty, vty-crossplatform
, wai, warp
}:
mkDerivation {
  pname = "xbs-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers d3x ihp-hsx lens linear text uu-parsinglib vector
    vector-algorithms
  ];
  executableHaskellDepends = [
    base bytestring oleg-delimcc d3x filepath http-types ihp-hsx stm text
    vector vty vty-crossplatform wai warp
  ];
  testHaskellDepends = [ base containers hspec linear text ];
  doCheck = false;   # tests read ../examples (not in the nix src); run via `cabal test`
  doHaddock = false;
  description = "Ball-and-stick molecular viewer rendered as SVG via d3x";
  license = lib.licenses.mit;
}
