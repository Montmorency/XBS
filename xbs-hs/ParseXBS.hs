{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsers for XBS input formats. Today: the native @.bs@ format. Later this
--   module will also host converters from .xyz/.exyz/.cif/.pdb into XBS's
--   'Ball'/'Bond' world. Built on uulib (error-correcting), mirroring the house
--   P6Parser style.
module ParseXBS
  ( BsLine(..)
  , parseBs
  , loadBs
  , parseMv
  ) where

import XBS

import           Data.Char  (isSpace)
import           Data.Text  (Text)
import qualified Data.Text  as T
import qualified Data.Map   as M
import           Text.Read  (readMaybe)
import           Linear     (V3(..))

import Text.ParserCombinators.UU                  -- P, parse, pEnd, (<|>), pMany, … (Applicative ops from Prelude)
import Text.ParserCombinators.UU.Utils            (pSymbol, pSpaces)
import Text.ParserCombinators.UU.BasicInstances   (Str, LineColPos(..), createStr, pMunch)

-- char-level parser over String (transparent synonym → UU.Utils combinators fit)
type Parser a = P (Str Char String LineColPos) a

-- | One recognised directive line of a @.bs@ file.
data BsLine = LAtom   Text Double Double Double                 -- ^ atom  sp x y z
            | LSpec   Text Double [Double]                      -- ^ spec  sp radius colour(1 gray | 3 rgb)
            | LBonds  Text Text Double Double Double [Double]    -- ^ bonds s1 s2 min max radius colour(1|3)
            | LTmat   [Double]                                  -- ^ tmat  (9 floats)
            | LScalar Text Double                               -- ^ scale/dist/inc/rfac/bfac …
            | LSwitches [Int]                                   -- ^ switches: pixmap numbers gray BLINE wire bonds recenter PMODE shadow
            | LIgnore                                           -- ^ '*' comments, line, blank, unknown
            deriving (Eq, Show)

-- a whitespace-delimited word (consumes leading spaces)
pWord :: Parser String
pWord = pSpaces *> pMunch (not . isSpace)

pText :: Parser Text
pText = T.pack <$> pWord

numChars :: String
numChars = "+-.eE0123456789"

-- Fortran-friendly number read: fix a leading '.' then read (accepts ".78",
-- "-.2349" which `read`/pDouble reject); 0 on garbage.
readNum :: String -> Double
readNum s = maybe 0 id (readMaybe (fixup s))
  where
    fixup ('-':'.':r) = "-0." ++ r
    fixup ('+':'.':r) = "0."  ++ r
    fixup ('.':r)     = "0."  ++ r
    fixup t           = t

-- a single number (munch the numeric chars). NB: pMunch can match EMPTY (→ 0),
-- so pNum may succeed consuming nothing — fine in sequence, but NEVER under
-- `pMany` (that loops: <<loop>>). For lists of numbers use pRestNums.
pNum :: Parser Double
pNum = readNum <$> (pSpaces *> pMunch (`elem` numChars))

-- all remaining whitespace-separated numbers on the line (e.g. tmat's 9 floats),
-- via munch-the-rest + words — avoids `pMany pNum`'s empty-loop.
pRestNums :: Parser [Double]
pRestNums = map readNum . words <$> pMunch (const True)

-- one line → a BsLine (keyword-dispatched; falls back to LIgnore)
-- NB: <<|> (greedy, left-biased) not <|>: the LIgnore catch-all succeeds on
-- every line at zero cost, so with symmetric <|> it can swallow real lines.
-- <<|> commits to the first branch that makes progress.
pLine :: Parser BsLine
pLine = pSpaces *>
   (    LAtom   <$  pSymbol "atom"  <*> pText <*> pNum <*> pNum <*> pNum
   <<|> LSpec   <$  pSymbol "spec"  <*> pText <*> pNum <*> pRestNums
   <<|> LBonds  <$  pSymbol "bonds" <*> pText <*> pText <*> pNum <*> pNum <*> pNum <*> pRestNums
   <<|> LTmat     <$  pSymbol "tmat"     <*> pRestNums
   <<|> LSwitches <$  pSymbol "switches" <*> (map round <$> pRestNums)
   <<|> LScalar   <$> pScalarKw <*> pNum
   <<|> LIgnore   <$  pMunch (const True)
   )
  where
    pScalarKw = T.pack <$> ( pSymbol "scale" <<|> pSymbol "dist" <<|> pSymbol "inc"
                        <<|> pSymbol "rfac"  <<|> pSymbol "bfac" )

execParser :: Parser a -> String -> a
execParser p inp = fst (parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp))

-- | Parse every line of a @.bs@ source into directives.
parseBs :: String -> [BsLine]
parseBs = map (execParser pLine) . lines

-- | Assemble parsed lines into XBS structures: a 'Config' (view matrix), the
--   'Ball's (atoms joined with their species' radius/gray), and a 'BondMap'.
loadBs :: String -> (Config, [Ball], BondMap)
loadBs src = (config, balls, bondMap)
  where
    ls = parseBs src

    -- species → (radius, colour values: 1 gray or 3 rgb)
    specs = M.fromList [ (sp, (r, cvals)) | LSpec sp r cvals <- ls ]

    balls = [ mkBall sp x y z | LAtom sp x y z <- ls ]
    mkBall sp x y z =
      let (r, cvals) = M.findWithDefault (1.0, [0.5]) sp specs
      in Ball { pos = V3 x y z, rad = r
              , gray = realToFrac (atDef cvals 0 0.5), rgb = toRGB cvals
              , col = 0, special = 0, species = sp }

    -- bonds are symmetric → insert both species orderings
    bondMap = M.fromList $ concat
      [ [ ((s1, s2), bnd), ((s2, s1), bnd) ]
      | LBonds s1 s2 mn mx r cvals <- ls
      , let bnd = Bond { sp1 = s1, sp2 = s2, minLength = mn, maxLength = mx
                       , radius = r, rgb = toRGB cvals } ]

    tmat0 = case [ ds | LTmat ds <- ls ] of
              (ds : _) -> toMat3 ds
              _        -> init_tmat
    -- switches: …gray BLINE(idx 3) wire bonds recenter PMODE(idx 7) shadow
    switches0 = case [ ss | LSwitches ss <- ls ] of (ss : _) -> ss; _ -> []
    bline0    = atDef switches0 3 0 == 1            -- idx 3 bline: 1 = lines, 0 = cylinders
    persp0    = atDef switches0 7 0 == 2            -- idx 7 pmode: 2 = true perspective
    config    = defConfig { tmat = tmat0, bline = bline0, perspective = persp0 }

-- | Parse a @.mv@ movie file into frames of atom positions. Each @frame …@
--   header line starts a new frame; the whitespace-separated floats that follow
--   (until the next header) are the atoms' x y z in the SAME order as the .bs
--   atoms. @natoms@ fixes the chunk size (trailing junk is dropped); frame 0 of
--   a movie equals the .bs coordinates. Lines before the first header are skipped.
parseMv :: Int -> String -> [[Vec3]]
parseMv natoms = map frameVecs . frames . lines
  where
    isHdr l = case words l of ("frame":_) -> True; _ -> False
    frames []                  = []
    frames (l:ls) | isHdr l    = let (body, rest) = break isHdr ls in body : frames rest
                  | otherwise  = frames ls          -- skip preamble before frame 1
    frameVecs body = take natoms (toV3s (map readNum (concatMap words body)))
    toV3s (x:y:z:r) = V3 x y z : toV3s r
    toV3s _         = []

-- safe list index with default
atDef :: [a] -> Int -> a -> a
atDef xs i d = case drop i xs of (x:_) -> x; _ -> d

-- the .bs colour field → RGB (1 value = gray, 3 = rgb)
toRGB :: [Double] -> RGB
toRGB [g]       = RGB g g g
toRGB (r:g:b:_) = RGB r g b
toRGB _         = RGB 0.5 0.5 0.5

toMat3 :: [Double] -> Mat3
toMat3 [a,b,c, d,e,f, g,h,i] = V3 (V3 a b c) (V3 d e f) (V3 g h i)
toMat3 _                     = init_tmat
