{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | xbs-render: load each .bs file and render it to an .svg next to it.
--     xbs-render examples/ch4.bs examples/c60.bs ...
--   With --dump, print the parsed directives + the backend-neutral Picture IR
--   instead (debugging, and for feeding other graphics backends):
--     xbs-render --dump examples/ch4.bs
module Main (main) where

import XBS
import ParseXBS              (loadBs, parseBs)
import D3X.Blocks            (svgViewBox)

import qualified Data.Vector   as V
import qualified Data.Text.IO  as T
import           IHP.HSX.Markup (renderMarkupText)
import           System.Environment (getArgs)
import           System.FilePath    (replaceExtension, takeFileName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--dump" : files) -> mapM_ dumpFile files
    files              -> mapM_ renderFile files

-- the backend-neutral (paper-space) Picture IR for a .bs source
pictureOf :: String -> Picture
pictureOf src =
  let (config, balls, bondMap) = loadBs src
      ballsAndSticks           = V.fromList (stick bondMap balls)
  in drawScene config config.tmat ballsAndSticks

renderFile :: FilePath -> IO ()
renderFile path = do
  src <- readFile path
  let svg = svgViewBox (round width) (round height) (renderSvg (pictureOf src))
      out = replaceExtension (takeFileName path) "svg"  -- write to CWD, not the source dir
  T.writeFile out (renderMarkupText svg)
  putStrLn ("wrote " <> out)

-- | Print parsed directives + the Picture IR (no SVG written).
dumpFile :: FilePath -> IO ()
dumpFile path = do
  src <- readFile path
  putStrLn ("=== " <> path <> " : parsed lines ===")
  mapM_ print (parseBs src)
  putStrLn "=== Picture IR ==="
  print (pictureOf src)
