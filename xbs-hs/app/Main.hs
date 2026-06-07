{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | xbs-render: load each .bs file, render it to an .svg next to it.
--   Usage: xbs-render examples/ch4.bs examples/c60.bs ...
module Main (main) where

import XBS
import ParseXBS              (loadBs)
import D3X.Blocks            (svgViewBox)

import qualified Data.Vector   as V
import qualified Data.Text.IO  as T
import           IHP.HSX.Markup (renderMarkupText)
import           System.Environment (getArgs)
import           System.FilePath    (replaceExtension)

main :: IO ()
main = getArgs >>= mapM_ renderFile

renderFile :: FilePath -> IO ()
renderFile path = do
  src <- readFile path
  let (config, balls, bondMap) = loadBs src
      ballsAndSticks           = V.fromList (stick bondMap balls)
      pic                      = drawScene config config.tmat ballsAndSticks
      svg                      = svgViewBox (round width) (round height) (renderSvg pic)
      out                      = replaceExtension path "svg"
  T.writeFile out (renderMarkupText svg)
  putStrLn ("wrote " <> out)
