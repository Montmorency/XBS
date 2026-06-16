{-# LANGUAGE OverloadedStrings #-}

-- | Terminal interpreter for the backend-neutral 'Picture' IR: rasterize to
--   Unicode braille so a pure-CLI user can orient without a browser. Kept apart
--   from "XBS" because the digitization is specialized — it follows METAFONT's
--   playbook (mf.web): shapes reduce to strokes, bonds are Bresenham lines
--   (ch.19 @make_moves@, straight-line case) and atoms are midpoint-circle
--   outlines (ch.25 octant-symmetric pen, with Knuth's minimum-size guard), and
--   the braille sub-dots stand in for MF's finer-than-device internal raster that
--   ch.27 @disp_edges@ packs to a device row by row.
--
--   Each glyph (@U+2800 + mask@) packs a 2×4 dot grid → 2×4 sub-cell resolution;
--   braille dots are ~square in a normal terminal cell, so a single uniform
--   fit-scale keeps atoms round (cf. 'XBS.viewportScale'). The dot bitmap is one
--   unboxed @Vector Word32@ (build O(dots+strokes), packing O(dots)); rows are
--   independent and yielded lazily, so output is ready to stream a row at a time.
--
--   Colour: each dot carries a packed RGB from its source prim. The z-sorted
--   back-to-front ordering means last-write-wins gives the frontmost atom's colour.
--   Per braille cell, the dominant colour is picked and returned alongside the
--   glyph character; the TUI builds vty Images with proper Attr colours from these.
module Braille (renderBraille, ColorMode(..), unpackRGB) where

import XBS (Picture(..), Prim(..), pictureExtent)

import qualified Data.Text             as T
import qualified Data.Map.Strict       as Map
import qualified Data.Vector.Unboxed   as VU
import           Data.Word             (Word32)
import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Data.List             (group, sortOn, maximumBy)
import           Data.Ord              (comparing, Down(..))
import           Linear                (V2(..))
import           D3X.Scales            (Domain(..))
import           Text.Read             (readMaybe)

-- | Braille colour mode, cycled by @c@ in the TUI controller.
data ColorMode = NoColor | GrayScale | FullColor
  deriving (Eq, Show, Enum, Bounded)

------------------------------------------------------------------------
-- Packed colour helpers
------------------------------------------------------------------------

-- | Pack RGB (0–255 each) into a Word32 with bit 24 set as on-flag.
--   0 is reserved for "dot off" so even black (0,0,0) becomes 0x01000000.
packColor :: Int -> Int -> Int -> Word32
packColor r g b = 0x01000000
  .|. (fromIntegral (clamp r) `shiftL` 16)
  .|. (fromIntegral (clamp g) `shiftL` 8)
  .|.  fromIntegral (clamp b)
  where clamp x = max 0 (min 255 x)

-- | Unpack a packed colour to (R, G, B) 0–255.
unpackRGB :: Word32 -> (Int, Int, Int)
unpackRGB w = ( fromIntegral ((w `shiftR` 16) .&. 0xFF)
              , fromIntegral ((w `shiftR` 8)  .&. 0xFF)
              , fromIntegral  (w              .&. 0xFF) )

-- | Parse our own @rgbToCss@ output (@"rgb(R,G,B)"@) back to a packed colour.
--   Falls back to light gray for @"none"@ (wireframe) or any unexpected format.
parseCssColor :: T.Text -> Word32
parseCssColor t = case T.stripPrefix "rgb(" t >>= T.stripSuffix ")" of
    Just inner -> case mapM (readMaybe . T.unpack . T.strip) (T.splitOn "," inner) of
        Just [r, g, b] -> packColor r g b
        _              -> fallback
    Nothing            -> fallback
  where fallback = packColor 180 180 180  -- light gray for wireframe / unknown

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

-- | Rasterize a 'Picture' to braille, returning rows of @(Char, Word32)@ pairs.
--   The Word32 is 0 for 'NoColor' mode, or a packed RGB (with on-flag in bit 24,
--   already grayscale-adjusted for 'GrayScale') that the caller can map to vty
--   attributes. @cols@×@rows@ are the character-cell dimensions of the target pane.
renderBraille :: ColorMode -> Int -> Int -> Picture -> [[(Char, Word32)]]
renderBraille colorMode cols rows pic
  | cols < 1 || rows < 1 = []
  | otherwise             = [ packRow cy | cy <- [0 .. rows-1] ]
  where
    dotW = 2*cols ; dotH = 4*rows

    -- Each dot is 0 (off) or a packed RGB with on-flag (nonzero).
    -- Back-to-front prim order + last-write-wins gives correct z-occlusion.
    grid :: VU.Vector Word32
    grid = VU.accum (\_ c -> c) (VU.replicate (dotW*dotH) 0)
             [ (y*dotW + x, c)
             | (x, y, c) <- rasterUpdates dotW dotH pic
             , x >= 0, x < dotW, y >= 0, y < dotH ]

    at x y = grid `VU.unsafeIndex` (y*dotW + x)

    packRow cy = [ cell cx cy | cx <- [0 .. cols-1] ]

    cell cx cy =
      let bx = 2*cx ; by = 4*cy
          -- Braille dot mask (same encoding as before)
          mask = bit  bx     by    0x01 + bit  bx    (by+1) 0x02
               + bit  bx    (by+2) 0x04 + bit  bx    (by+3) 0x40
               + bit (bx+1)  by    0x08 + bit (bx+1) (by+1) 0x10
               + bit (bx+1) (by+2) 0x20 + bit (bx+1) (by+3) 0x80
          bit x y v = if at x y /= 0 then v else (0 :: Int)
          ch = toEnum (0x2800 + mask) :: Char
          col = case colorMode of
                  NoColor -> 0
                  _       -> cellColor colorMode bx by
      in (ch, col)

    -- Pick the dominant colour from the 8 dots in this 2×4 cell.
    -- Applies grayscale conversion here so the caller just sees final RGB.
    cellColor mode bx by =
      let dots = [ at (bx+dx) (by+dy)
                 | dy <- [0..3], dx <- [0..1]
                 , at (bx+dx) (by+dy) /= 0 ]
      in case dots of
           [] -> 0
           _  -> let dominant = mostCommon dots
                     (r, g, b) = unpackRGB dominant
                 in case mode of
                      FullColor -> dominant
                      _         -> let l = lumin r g b in packColor l l l

-- | ITU-R BT.601 luminance (the standard RGB→gray conversion).
lumin :: Int -> Int -> Int -> Int
lumin r g b = round (0.299 * fromIntegral r + 0.587 * fromIntegral g
                                            + 0.114 * fromIntegral b :: Double)

-- | Most frequent element in a non-empty list.
mostCommon :: [Word32] -> Word32
mostCommon xs = case maximumBy (comparing length) (group (sortOn Down xs)) of
                  (c:_) -> c
                  []    -> 0  -- unreachable: group produces non-empty sublists

------------------------------------------------------------------------
-- Raster updates (with colour)
------------------------------------------------------------------------

-- | Per-prim raster updates in Picture order (already z-sorted, back to front).
--   Each update is @(x, y, packedColor)@ where 0 = clear (moat) and nonzero =
--   dot-on with the prim's colour. Last-write-wins in 'VU.accum' gives correct
--   z-occlusion: a solid atom first clears a one-dot-wider disc (the "moat":
--   occludes back geometry AND leaves a separating ring) then fills with colour.
rasterUpdates :: Int -> Int -> Picture -> [(Int, Int, Word32)]
rasterUpdates dotW dotH pic@(Picture prims) = concatMap draw prims
  where
    (sc, cx0, cy0) = fitScale dotW dotH pic
    mx = fromIntegral dotW / 2 ; my = fromIntegral dotH / 2
    rx x = round (mx + sc*(x - cx0)) :: Int
    ry y = round (my - sc*(y - cy0)) :: Int     -- flip y (screen points down)
    paint c = map (\(x,y) -> (x, y, c))
    draw (Disc _ cx cy r fill)
      | fill == "none" = paint (parseCssColor "none") (circlePts cX cY rr)  -- wireframe outline
      | otherwise      = paint 0             (disk cX cY (rr+1))            -- moat: clear
                      ++ paint (parseCssColor fill) (disk cX cY rr)         -- solid ball
      where cX = rx cx ; cY = ry cy ; rr = max 1 (round (sc*r))
    draw (Polyline ps col)  = paint (parseCssColor col) (stroke ps)         -- thin bond
    draw (Polygon  ps fill)
      | fill == "none" = paint (parseCssColor "none") (stroke (close ps))   -- outline
      | otherwise      = paint (parseCssColor fill)   (fillSpans (stroke (close ps)))
    draw (Label _ _ _) = []
    close ps  = ps ++ take 1 ps
    stroke ps = concat (zipWith seg ps (drop 1 ps))
    seg (V2 x0 y0) (V2 x1 y1) = bresenham (rx x0, ry y0) (rx x1, ry y1)

------------------------------------------------------------------------
-- Geometry (unchanged)
------------------------------------------------------------------------

-- | A filled disc: the midpoint-circle boundary, then fill each scanline between its
--   extremes (the convex case of METAFONT's per-row edge fill, mf.web ch.20/22).
disk :: Int -> Int -> Int -> [(Int,Int)]
disk cx cy r = fillSpans (circlePts cx cy r)

-- | Convex scanline fill: span each row from its leftmost to rightmost boundary x.
--   Exact for discs and convex bond quads (our only filled shapes).
fillSpans :: [(Int,Int)] -> [(Int,Int)]
fillSpans pts =
  [ (x,y) | (y,(lo,hi)) <- Map.toList spans, x <- [lo .. hi] ]
  where
    spans = Map.fromListWith (\(a,b) (c,d) -> (min a c, max b d))
                             [ (y, (x,x)) | (x,y) <- pts ]

-- | Uniform paper→dot scale fitting the Picture's extent into @dotW@×@dotH@ with a
--   one-dot margin, plus the data centre. Same fit philosophy as 'XBS.viewportScale'.
fitScale :: Int -> Int -> Picture -> (Double, Double, Double)
fitScale dotW dotH pic = case pictureExtent pic of
    Nothing -> (1, 0, 0)
    Just (Domain (mnx,mxx), Domain (mny,mxy)) ->
      let spanx = max 1e-9 (mxx - mnx)
          spany = max 1e-9 (mxy - mny)
          s = min (fromIntegral (dotW-2) / spanx) (fromIntegral (dotH-2) / spany)
      in (s, 0.5*(mnx+mxx), 0.5*(mny+mxy))

-- | Midpoint circle outline (8-fold octant symmetry; mf.web ch.25). @r<1@ collapses
--   to the centre dot — Knuth's minimum-size guard so far/small atoms still show.
circlePts :: Int -> Int -> Int -> [(Int,Int)]
circlePts cx cy r
  | r < 1     = [(cx, cy)]
  | otherwise = concatMap oct (go 0 r (1 - r) [])
  where
    oct (x,y) = [ (cx+x,cy+y),(cx-x,cy+y),(cx+x,cy-y),(cx-x,cy-y)
                , (cx+y,cy+x),(cx-y,cy+x),(cx+y,cy-x),(cx-y,cy-x) ]
    go x y d acc
      | x > y     = acc
      | otherwise = let acc' = (x,y) : acc
                        (y',d') | d < 0     = (y,   d + 2*x + 3)
                                | otherwise = (y-1, d + 2*(x-y) + 5)
                    in go (x+1) y' d' acc'

-- | Integer Bresenham line — the straight-line digitization of mf.web ch.19's
--   @make_moves@ (rook moves, unit steps only).
bresenham :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
bresenham (x0,y0) (x1,y1) = go x0 y0 (dx - dy) []
  where
    dx = abs (x1-x0) ; dy = abs (y1-y0)
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    go x y e acc
      | x == x1 && y == y1 = (x,y) : acc
      | otherwise =
          let e2       = 2*e
              (ex, x') = if e2 > negate dy then (e - dy, x + sx) else (e, x)
              (ey, y') = if e2 < dx        then (ex + dx, y + sy) else (ex, y)
          in go x' y' ey ((x,y) : acc)
