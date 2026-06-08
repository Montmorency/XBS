{
  description = "XBS rosetta stone — the ball-and-stick molecular viewer in three tongues: the original C (xbs-c), a Haskell port over d3x (xbs-hs), and a browser spec (xbs-js)";

  inputs = {
    # ihp pins nixpkgs AND supplies ihp-hsx 1.6 from its master source tree.
    # We can't take ihp-hsx from the nixpkgs Hackage snapshot: that's still
    # 1.5.0 (blaze backend + IHP.HSX.Parser only), and d3x needs the 1.6
    # ByteString-Builder backend (IHP.HSX.Markup / IHP.HSX.MarkupQQ).
    ihp.url = "github:digitallyinduced/ihp";
    nixpkgs.follows = "ihp/nixpkgs";
    # d3x supplies the scale typeclasses, d3Line primitives, etc. that xbs-hs
    # builds on. Sourced as a sibling checkout; share ihp + nixpkgs so the
    # Haskell package set stays coherent across the whole tree.
        d3x = {
            url = "path:/Users/lambert/projects/ihp-projects/d3x";
            flake = false;
        };
    # CC-delcont-ref (multi-prompt delimited control via reference cells —
    # Oleg's reference impl of Dybvig/Peyton Jones/Sabry, mirroring delimcc's
    # OCaml interface) for xbs-live's input loop. Built from the local checkout;
    # we ship this ourselves rather than depend on nixpkgs' broken CC-delcont.
    cc-delcont-ref = {
        url = "path:/Users/lambert/projects/CCRef";
        flake = false;
    };
  };

  outputs =
    { self, nixpkgs, ihp, d3x, cc-delcont-ref }:
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
            # self.overlays.default adds ihp-hsx 1.6, d3x and xbs-hs to
            # haskellPackages via committed default.nix files (no IFD).
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          }
        );
    in
    {
      # Inject ihp-hsx 1.6, d3x and xbs-hs into haskellPackages following IHP's
      # no-IFD convention: callPackage a committed cabal2nix output rather than
      # callCabal2nix. ihp-hsx's source is IHP's master tree (the `ihp` input);
      # d3x's source comes from its flake input; xbs-hs is local. The cabal files
      # are the spec; the default.nix files mirror them (kept in lockstep via
      # `nix run nixpkgs#cabal2nix`).
      #
      # All three go through this one haskellPackages set, so they share the same
      # profiling settings — no mismatch between ihp-hsx and its dependents.
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
            hfinal: hprev: {
              # Build ihp-hsx 1.6 from the pinned IHP source. The committed
              # default.nix labels itself version "1.5.0" (stale cabal2nix
              # metadata) but `src = ./.` is the 1.6.0 tree, so GHC registers it
              # as 1.6.0 — satisfying d3x's `ihp-hsx >= 1.6` bound.
              ihp-hsx = hfinal.callPackage "${ihp}/ihp-hsx/default.nix" { };
              d3x = hfinal.callPackage "${d3x}/default.nix" { };
              # local CC-delcont-ref (Oleg's reference impl); we ship it
              # ourselves rather than use nixpkgs' broken CC-delcont
              CC-delcont-ref = hfinal.callPackage "${cc-delcont-ref}/default.nix" { };
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
