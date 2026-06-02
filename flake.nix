{
  description = "XBS rosetta stone — the ball-and-stick molecular viewer in three tongues: the original C (xbs-c), a Haskell port over d3x (xbs-hs), and a browser spec (xbs-js)";

  inputs = {
    # ihp is used only to pin nixpkgs (its Hackage snapshot already carries
    # ihp-hsx, so we don't need ihp's own overlay).
    ihp.url = "github:digitallyinduced/ihp";
    nixpkgs.follows = "ihp/nixpkgs";

    # d3x supplies the scale typeclasses, d3Line primitives, etc. that xbs-hs
    # builds on. Sourced as a sibling checkout; share ihp + nixpkgs so the
    # Haskell package set stays coherent across the whole tree.
    d3x.url = "path:../../ihp-projects/d3x";
    d3x.inputs.ihp.follows = "ihp";
    d3x.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self, nixpkgs, ihp, d3x }:
    let
      allSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs allSystems (
          system:
          f {
            inherit system;
            # self.overlays.default adds both d3x and xbs-hs to haskellPackages
            # via committed default.nix files (no IFD). ihp-hsx is resolved from
            # the pinned nixpkgs Hackage snapshot.
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          }
        );
    in
    {
      # Inject d3x and xbs-hs into haskellPackages following IHP's no-IFD
      # convention: callPackage a committed cabal2nix output rather than
      # callCabal2nix. d3x's source comes from the flake input; xbs-hs is local.
      # The cabal files are the spec; the default.nix files mirror them (kept in
      # lockstep via `nix run nixpkgs#cabal2nix`). ihp-hsx resolves from nixpkgs.
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
            hfinal: hprev: {
              d3x = hfinal.callPackage "${d3x}/default.nix" { };
              xbs-hs = hfinal.callPackage ./xbs-hs/default.nix { };
            }
          );
        });
      };

      packages = forAllSystems (
        { pkgs, system }:
        {
          # --- xbs-c: the original X11 C implementation -------------------
          # Compiled directly rather than via ./Makefile, whose `CFLAGS = -O3 -I`
          # expands to `gcc -O3 -I -o xbs ...` (the `-o` is swallowed as the
          # include dir). Nix injects X11 include/lib paths via buildInputs.
          xbs-c = pkgs.stdenv.mkDerivation {
            pname = "xbs-c";
            version = "1.0";
            src = ./.;
            buildInputs = [ pkgs.xorg.libX11 ];
            buildPhase = ''
              $CC -O3 -o xbs xbs.c -lX11 -lm
            '';
            installPhase = ''
              install -Dm755 xbs $out/bin/xbs
            '';
          };

          # --- xbs-hs: Haskell port over d3x ------------------------------
          xbs-hs = pkgs.haskellPackages.xbs-hs;

          # --- xbs-js: browser spec, bundled with Deno --------------------
          # For now xbs.js is an Observable-flavored spec; the bundle step is a
          # placeholder until it's tweaked to run standalone. We install the
          # source + deno manifest so the artifact is addressable today.
          xbs-js = pkgs.stdenv.mkDerivation {
            pname = "xbs-js";
            version = "0.1.0";
            src = ./xbs-js;
            nativeBuildInputs = [ pkgs.deno ];
            buildPhase = ''
              export DENO_DIR="$TMPDIR/deno-cache"
              # TODO: once xbs.js runs outside Observable, bundle it:
              #   deno bundle xbs.js xbs.bundle.js
            '';
            installPhase = ''
              mkdir -p $out/lib/xbs-js
              cp xbs.js README.md deno.json $out/lib/xbs-js/
            '';
          };

          default = self.packages.${system}.xbs-c;
        }
      );

      devShells = forAllSystems (
        { pkgs, system }:
        {
          # Default = the Haskell shell (next step is build/test xbs-hs).
          # shellFor pulls d3x, ihp-hsx and the rest of xbs-hs's deps into scope.
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.xbs-hs ];
            nativeBuildInputs = with pkgs.haskellPackages; [
              cabal-install
              haskell-language-server
            ];
          };

          xbs-c = pkgs.mkShell {
            packages = [ pkgs.gcc pkgs.pkg-config pkgs.xorg.libX11 ];
          };

          xbs-js = pkgs.mkShell {
            packages = [ pkgs.deno ];
          };
        }
      );
    };
}
