{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Parser tests, grown modularly as ParseXBS gains formats.
module Main (main) where

import Test.Hspec

import XBS
import ParseXBS

import           Linear      (V3(..))
import           Data.Maybe  (isJust)
import qualified Data.Map    as M

-- mirrors examples/ch4.bs (kept inline so the test is path-independent)
ch4 :: String
ch4 = unlines
  [ "atom      C       0.000      0.000      0.000 "
  , "atom      H       1.155      1.155      1.155 "
  , "atom      H      -1.155     -1.155      1.155 "
  , "atom      H       1.155     -1.155     -1.155 "
  , "atom      H      -1.155      1.155     -1.155 "
  , ""
  , "spec      C      1.000   0.7"
  , "spec      H      0.700   1.00 "
  , ""
  , "bonds     C     C    0.000    4.000    0.109   1.00 "
  , "bonds     C     H    0.000    3.400    0.109   1.00 "
  , "bonds     H     H    0.000    2.800    0.109   1.00 "
  , ""
  , "tmat  1.000  0.000  0.000  0.000  1.000  0.000  0.000  0.000  1.000"
  , "dist    12.000"
  , "scale   20.000"
  , "switches 1 0 1 0 0 1 1 0 0"
  ]

main :: IO ()
main = hspec $ do

  describe "parseBs — individual .bs directive lines" $ do
    it "parses an atom line (species + xyz)" $
      parseBs "atom C 0.0 1.5 -2.0" `shouldBe` [LAtom "C" 0.0 1.5 (-2.0)]
    it "accepts Fortran-style numbers (.78, -.5)" $
      parseBs "atom O .78 -.5 1.0" `shouldBe` [LAtom "O" 0.78 (-0.5) 1.0]
    it "parses a spec line (radius + gray)" $
      parseBs "spec C 1.0 0.7" `shouldBe` [LSpec "C" 1.0 0.7]
    it "parses a bonds line" $
      parseBs "bonds C H 0.0 3.4 0.109 1.0" `shouldBe` [LBonds "C" "H" 0.0 3.4 0.109 1.0]
    it "parses a tmat line (9 floats)" $
      parseBs "tmat 1 0 0 0 1 0 0 0 1" `shouldBe` [LTmat [1,0,0, 0,1,0, 0,0,1]]
    it "ignores comments / unknown directives" $
      parseBs "* a comment" `shouldBe` [LIgnore]

  describe "loadBs — ch4" $ do
    let (_config, balls, bondMap) = loadBs ch4

    it "reads all 5 atoms" $
      length balls `shouldBe` 5

    it "has one C and four H" $ do
      length [ b | b <- balls, b.species == "C" ] `shouldBe` 1
      length [ b | b <- balls, b.species == "H" ] `shouldBe` 4

    it "places C at the origin with its spec radius (1.0)" $ do
      let c = head [ b | b <- balls, b.species == "C" ]
      c.pos `shouldBe` V3 0 0 0
      c.rad `shouldBe` 1.0

    it "joins H atoms with the H spec radius (0.7)" $ do
      let h = head [ b | b <- balls, b.species == "H" ]
      h.rad `shouldBe` 0.7
      h.pos `shouldBe` V3 1.155 1.155 1.155

    it "builds a C–H bond rule with the right bounds/radius" $ do
      let Just b = M.lookup ("C", "H") bondMap
      b.minLength `shouldBe` 0.0
      b.maxLength `shouldBe` 3.4
      b.radius    `shouldBe` 0.109

    it "stores bond rules symmetrically (H–C too)" $
      isJust (M.lookup ("H", "C") bondMap) `shouldBe` True

  describe "loadBs — ring.bs (cage, 60 C + 2 O)" $ do
    -- reads the real example file; cabal test runs with cwd = xbs-hs/
    src <- runIO (readFile "../examples/ring.bs")
    let (_config, balls, bondMap) = loadBs src

    it "reads all 62 atoms" $
      length balls `shouldBe` 62

    it "has 60 C and 2 O" $ do
      length [ b | b <- balls, b.species == "C" ] `shouldBe` 60
      length [ b | b <- balls, b.species == "O" ] `shouldBe` 2

    it "carries per-species gray (C .67, O .50)" $ do
      (head [ b | b <- balls, b.species == "C" ]).gray `shouldSatisfy` approx 0.67
      (head [ b | b <- balls, b.species == "O" ]).gray `shouldSatisfy` approx 0.50

    it "reads the first atom's position (exact)" $
      (head balls).pos `shouldBe` V3 (-1.314) (-0.015) 6.623

    it "reads the last atom (an O) position (exact)" $
      (last balls).pos `shouldBe` V3 8.050 0.0 3.040

    it "builds a C–O bond rule (max 3, radius 0.143), both orderings" $ do
      let Just b = M.lookup ("C", "O") bondMap
      b.maxLength `shouldBe` 3.0
      b.radius    `shouldBe` 0.143
      isJust (M.lookup ("O", "C") bondMap) `shouldBe` True

    it "has 4 distinct bond-map keys (C-C, C-O, O-C, O-O)" $
      M.size bondMap `shouldBe` 4

-- gray is stored as Float (realToFrac from the parsed Double), so compare ~approximately
approx :: Float -> Float -> Bool
approx target x = abs (x - target) < 1e-4
