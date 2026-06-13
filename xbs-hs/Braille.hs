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
--   unboxed @Vector Bool@ (build O(dots+strokes), packing O(dots)); rows are
--   independent and yielded lazily, so output is ready to stream a row at a time.
module Braille (renderBraille) where

import XBS (Picture(..), Prim(..), pictureExtent)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Map.Strict       as Map
import qualified Data.Vector.Unboxed   as VU
import           Linear                (V2(..))
import           D3X.Scales            (Domain(..))

-- | Rasterize a 'Picture' to braille, one 'Text' per character row.
--   @cols@×@rows@ are the character-cell dimensions of the target pane.
renderBraille :: Int -> Int -> Picture -> [Text]
renderBraille cols rows pic
  | cols < 1 || rows < 1 = []
  | otherwise            = [ packRow cy | cy <- [0 .. rows-1] ]
  where
    dotW = 2*cols ; dotH = 4*rows
    grid :: VU.Vector Bool
    grid = VU.accum (\_ b -> b) (VU.replicate (dotW*dotH) False)
             [ (y*dotW + x, b)
             | (x,y,b) <- rasterUpdates dotW dotH pic
             , x >= 0, x < dotW, y >= 0, y < dotH ]
    on x y = grid `VU.unsafeIndex` (y*dotW + x)
    packRow cy = T.pack [ glyph cx cy | cx <- [0 .. cols-1] ]
    glyph cx cy = toEnum (0x2800 + mask) :: Char
      where bx = 2*cx ; by = 4*cy
            mask = bit  bx     by    0x01 + bit  bx    (by+1) 0x02
                 + bit  bx    (by+2) 0x04 + bit  bx    (by+3) 0x40
                 + bit (bx+1)  by    0x08 + bit (bx+1)(by+1) 0x10
                 + bit (bx+1) (by+2) 0x20 + bit (bx+1)(by+3) 0x80
            bit x y v = if on x y then v else (0 :: Int)

-- | Per-prim raster updates in Picture order (already z-sorted, back to front), as
--   @(x, y, on?)@ so 'renderBraille' can apply them last-write-wins and let front
--   atoms occlude what's behind. A solid atom first clears a one-dot-wider disc
--   (the "moat": occludes back geometry AND leaves a separating ring) then fills.
--   Hollow atoms (fill @"none"@ = wireframe) stay outlines and thin bonds stay
--   strokes, so the SVG's @w@ (wire) and @l@ (line) toggles carry through unchanged.
rasterUpdates :: Int -> Int -> Picture -> [(Int,Int,Bool)]
rasterUpdates dotW dotH pic@(Picture prims) = concatMap draw prims
  where
    (sc, cx0, cy0) = fitScale dotW dotH pic
    mx = fromIntegral dotW / 2 ; my = fromIntegral dotH / 2
    rx x = round (mx + sc*(x - cx0)) :: Int
    ry y = round (my - sc*(y - cy0)) :: Int     -- flip y (screen points down)
    paint b = map (\(x,y) -> (x, y, b))
    draw (Disc _ cx cy r fill)
      | fill == "none" = paint True (circlePts cX cY rr)             -- wireframe: outline
      | otherwise      = paint False (disk cX cY (rr+1))             -- moat: occlude + ring
                      ++ paint True  (disk cX cY rr)                 -- solid ball
      where cX = rx cx ; cY = ry cy ; rr = max 1 (round (sc*r))
    draw (Polyline ps _)   = paint True (stroke ps)                  -- thin bond
    draw (Polygon  ps fill)
      | fill == "none" = paint True (stroke (close ps))              -- outline
      | otherwise      = paint True (fillSpans (stroke (close ps)))  -- solid bond
    close ps  = ps ++ take 1 ps
    stroke ps = concat (zipWith seg ps (drop 1 ps))
    seg (V2 x0 y0) (V2 x1 y1) = bresenham (rx x0, ry y0) (rx x1, ry y1)

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
