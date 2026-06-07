{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module XBS where

import D3X.Prelude
import D3X.Blocks hiding (dot)  -- dot = Linear.dot (vector) here, not the SVG circle
import D3X.Scales
import D3X.Path (d3Line, PathStr)

import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html)

import Linear hiding (perspective)  -- we define our own render-mode `perspective`
import Linear.Quaternion (rotate)
import Linear.Plucker    (Plucker(..))  -- used as a convenient 6-element carrier

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VAM  -- stable merge sort

import           Data.Ord     (comparing)
import           Data.Maybe   (catMaybes)
import           Data.List    (sortOn)
import           Control.Lens ((^.))
import           Linear.V3    (V3, _z)

-- XBS Top Types
type Perspective = Bool

-- Monomorphise linear's parametric V3/M33 to Double for all XBS geometry.
-- Value-level `V3 x y z` constructors stay; these aliases are for type positions.
type Vec3 = V3 Double          -- a 3-vector of coordinates
type Mat3 = M33 Double         -- = V3 (V3 Double), a 3x3 transform matrix

data Config = Config { bline :: Bool
                     , dist0 :: Float
                     , scale :: Float
                     , tmat :: Mat3
                     }

data Ball = Ball { pos :: Vec3
                 , rad :: Radius
                 , gray :: Color
                 , rgb :: Color
                 , col :: Int
                 , special :: Int
                 , species :: Text
                 }
                          
data Stick = Stick { start :: Ball
                   , end :: Ball
                   , rad :: Radius
                   , gray :: Float
                   , col :: Int
                   }


type SpeciesName = Text
  
type MinLength = Double   -- physics: compared against model-space bond lengths (Double)
type MaxLength = Double   -- physics: ditto
type Radius    = Double   -- physics-adjacent: feeds the projection geometry
type Color     = Float    -- presentation: gray/colour value

data Bond = Bond { sp1       :: SpeciesName
                 , sp2       :: SpeciesName
                 , minLength :: MinLength
                 , maxLength :: MaxLength
                 , radius    :: Radius
                 , gray      :: Color      -- the .bs "color" field is a gray 0..1 (feeds Stick.gray)
                 }

-- Backend-neutral drawing IR. The geometry fold produces a Picture (a free
-- monoid of primitives); each backend has its own interpreter (renderSvg via
-- d3x, renderCanvas via blank-canvas, ...). Nothing SVG/canvas-specific here.
data Prim = Disc     Double Double Double Text  -- ^ cx cy r  fill   (atom)
          | Polyline [V2 Double] Text           -- ^ pts      stroke (thin bond)
          | Polygon  [V2 Double] Text           -- ^ pts      fill   (thick bond)
          deriving (Eq, Show)

newtype Picture = Picture [Prim]
  deriving (Eq, Show, Semigroup, Monoid)

defConfig = Config { bline = True
                   , dist0 = 250
                   , scale = 15
                   }
-- XBS constants: these should get plugged into config for interaction (versor drag or terminal connection)
-- kitty render?
maxBond = 16 
dist0 = 250
d = V3 0 0 dist0
dfac = 0.8
rfac = 2.0
npoints = 5

init_tmat :: Mat3
init_tmat = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)  -- M33 a = V3 (V3 a); no M33 constructor

height = 450
width = 450
left_margin = 20
right_margin = 20
top_margin = 20
bottom_margin = 20
dalfa = 0.08726646259971647
bndfac = 1
-- render globals (C uses these as globals; Config mirrors them for later threading)
maxRad = 100                -- C MAXRAD: cap on projected radius
scale = 15                  -- C `fac`: overall plot scale
perspective :: Perspective  -- C pmode==1 "pseudo" perspective; False = projection branch
perspective = False


-- This is to accumulate the atomic ordering from bottom to top along
------- sort atoms back to front ----- 
-- sort balls (with their sticks) back to front by their rotated z
sortByZ :: Mat3 -> V.Vector (Ball, [Stick]) -> V.Vector (Ball, [Stick])
sortByZ tmat = V.modify (VAM.sortBy (comparing (\(ball, _) -> (tmat !* ball.pos) ^. _z)))

phiVec i = let phi = i*pi/(npoints-1) in V2 (-1.0*(sin phi)) (cos phi)
arcs = phiVec <$> [0 .. npoints-1]



checkZero vec = if (abs(norm vec) < 0.0001)
                   then V3 1 0 0
                   else vec

-- TODO(scaling): bond/atom coords still need to pass through xScale/yScale into
-- the SVG viewport before d3Line. Sketch (clashed with D3X.Blocks.line, unused):
--   toViewport bonds = d3Line $
--     map (\a -> V2 (coordVal (xScale minX maxX (a ^. _x)))
--                   (coordVal (yScale minY maxY (a ^. _y)))) bonds

-- | Project an atom to paper: returns V3 (paperX) (paperY) (projectedRadius).
--   Faithful port of C atompos (subs.h ~189); render mode passed in (C pmode),
--   `scale`/`dist0` still globals for now.
atomPos :: Perspective -> Vec3 -> Radius -> Vec3
atomPos persp locP rad
  | persp =                             -- C pmode==1 "pseudo" perspective (subs.h ~196)
      let denom = dist0 - locP ^. _z
          zr | denom > 0 = min (scale * rad * dist0 / denom) maxRad
             | otherwise = maxRad
      in V3 (scale * locP ^. _x) (scale * locP ^. _y) zr
  | otherwise =                         -- C projection branch (subs.h ~206)
      let q = locP ^-^ d                -- d = V3 0 0 dist0
          y = checkZero (locP ^-^ ((dot locP q / quadrance q) *^ q))
          a = negate (rad*rad) / quadrance q
          b = rad * sqrt ((1 + a) / quadrance y)
          V3 v1x v1y v1z = locP ^+^ (a *^ q ^+^ b *^ y)
          V3 v2x v2y v2z = locP ^+^ (a *^ q ^-^ b *^ y)
          za1 = scale*v1x*dist0 / (dist0 - v1z)
          za2 = scale*v1y*dist0 / (dist0 - v1z)
          zb1 = scale*v2x*dist0 / (dist0 - v2z)
          zb2 = scale*v2y*dist0 / (dist0 - v2z)
          zr  = 0.5 * sqrt ((zb1-za1)^2 + (zb2-za2)^2)
      in V3 (0.5*(za1+zb1)) (0.5*(za2+zb2)) zr
  
-- | Core fold step: render one atom + its bonds into the Picture IR.
--   Applies the tmat rotation here (so sorting and drawing share view-space
--   coords), then z-sorts this atom's bonded neighbours back-to-front (the
--   rotation can change their relative depth).
drawBallAndSticks :: Config -> Mat3 -> (Ball, [Stick]) -> Picture
drawBallAndSticks config tmat (ball, sticks) =
    plotAtom (rot ball) <> foldMap drawOneBond sortedSticks
  where
    rot b        = b { pos = tmat !* b.pos }              -- ball into view space
    rotStick stk = stk { end = rot stk.end }              -- only .end is used downstream
    sortedSticks = sortOn (\stk -> (rot stk.end).pos ^. _z) sticks
    drawOneBond  = plotBond config (rot ball) . rotStick

-- | One bond's geometry as Picture primitives (points only; d3Line/SVG lives in
--   the interpreter). `stick` already visits each bond once, and the C/JS note
--   gate is effectively always-draw (it only chose draw-timing), so we draw
--   unconditionally — keeping just the degenerate-projection skip (C: xx²<1e-4).
plotBond :: Config -> Ball -> Stick -> Picture
plotBond config ballk sortedStick = let
         ballkk = sortedStick.end

         -- paper-space bond delta (C bs_kernel: zp[kk]-zp[k] via atompos),
         -- normalised to the unit screen bond direction (C: bx/=xx, by/=xx).
         V3 bdx bdy _ = atomPos perspective ballkk.pos ballkk.rad
                    ^-^ atomPos perspective ballk.pos  ballk.rad
         xx = sqrt (bdx^2 + bdy^2)
         bx = bdx / xx
         by = bdy / xx

         q1 = d ^-^ ballk.pos
         q2 = d ^-^ ballkk.pos
         -- 3D model-space bond vector (C bs_kernel: b[3] = p[kk]-p[k])
         b = ballkk.pos ^-^ ballk.pos

         -- cosines of the view/bond angle at each end (C: cth1, cth2 — note cth2's sign)
         cth1 =        dot q1 b  / sqrt (quadrance q1 * quadrance b)
         cth2 = negate (dot q2 b) / sqrt (quadrance q2 * quadrance b)

         br = bndfac * sortedStick.rad    -- C: br = bndfac*stick[ib].rad

         -- the two bond-cap 6-vectors (C m1[]/m2[]); caps offset toward each other
         Plucker m10 m11 m12 m13 m14 m15 = calcM perspective KEnd  bx by br ballk  cth1
         Plucker m20 m21 m22 m23 m24 m25 = calcM perspective KKEnd bx by br ballkk cth2
  in if xx*xx < 0.0001            -- atoms project to ~the same point: skip
       then mempty
       else if config.bline
              then Picture [Polyline [V2 m14 m15, V2 m24 m25] "black"] -- straight thin line
              else Picture [Polygon                                    -- thick tapered cap outline
                   ( map (\(V2 arcX arcY) -> V2 (m10*arcX + m12*arcY + m14)
                                                (m11*arcX + m13*arcY + m15)) arcs
                  ++ map (\(V2 arcX arcY) -> V2 (m20*arcX + m22*arcY + m24)
                                                (m21*arcX + m23*arcY + m25)) (reverse arcs)
                   ) "black"]

-- | Per-end bond-cap 6-vector, mirroring C bs_kernel's m1[]/m2[] (subs.h ~1248).
--   bx,by,br are bond-level (need both atoms + the stick), so they stay params;
--   rk and the projected (kx,ky,zk) are unpacked from this end's ball.
--   cth = cos of the view/bond angle at this end.
-- which end of the bond a cap belongs to; toggles the sign of the ww offset so
-- the two caps move TOWARD each other (C: +bx*ww at k, -bx*ww at kk).
data BondEnd = KEnd | KKEnd

calcM :: Perspective                 -- render mode (threaded to atomPos)
      -> BondEnd                     -- which end (sets the ww-offset sign)
      -> Double -> Double -> Double  -- bx by br  (shared bond quantities)
      -> Ball                        -- this end's atom (gives rk + its projection)
      -> Double                      -- cth
      -> Plucker Double
calcM persp end bx by br ball cth =
  let rk          = ball.rad
      V3 kx ky zk = atomPos persp ball.pos ball.rad   -- paper (x,y) + projected radius (zr)
      w   = sqrt (rk*rk - br*br)   -- C uses sqrt here; the port had dropped it
      sth = sqrt (1.0 - cth*cth)
      ww  = w * sth * zk / rk
      bb  = br * zk / rk
      aa  = br * cth * zk / rk
      off = case end of KEnd -> ww; KKEnd -> negate ww   -- C: zp[k]+bx*ww vs zp[kk]-bx*ww
  in Plucker (bx*aa) (by*aa) (-by*bb) (bx*bb) (kx + bx*off + taux) (ky + by*off + tauy)

tauy = 0.0
taux = 0.0
fudgefac = 0.6
gslope = 0
gz0 = 0      -- C gz0: gray-ramp z0 (set via `gramp`; default 0)
 
type BondMap = M.Map (Text, Text) Bond
stick :: BondMap -> [Ball] -> [(Ball, [Stick])]
stick _ [] = []
stick bondMap (ball1 : balls) = (ball1, catMaybes (map (\ball2 -> case M.lookup (ball1.species, ball2.species) bondMap of
                                             Nothing -> Nothing
                                             Just bond -> let
                                                 dis = norm (ball1.pos ^-^ ball2.pos)
                                               in
                                                 if ((dis >= bond.minLength) && dis <= bond.maxLength)
                                                 then Just (Stick { start = ball1, end = ball2
                                                                  , rad = bond.radius
                                                                  , gray = bond.gray
                                                                  , col = 0 })  -- palette index unused (b/w)
                                                 else Nothing) balls)) : (stick bondMap balls)

-- | Whole scene → Picture: z-order atoms back to front, then foldMap the core
--   step over them. Backend-neutral; pick an interpreter (renderSvg/…) after.
drawScene :: Config -> Mat3 -> V.Vector (Ball, [Stick]) -> Picture
drawScene config tmat = foldMap (drawBallAndSticks config tmat) . sortByZ tmat

-- | One atom as a Disc primitive (ball is expected already in view space).
plotAtom :: Ball -> Picture
plotAtom ball =
  let V3 px py pr = atomPos perspective ball.pos ball.rad   -- paper x,y + projected radius
  in Picture [Disc (px + taux) (py + tauy) pr "gray"]       -- TODO: fill from ball.gray/rgb

-- d3-style paper→viewport scales: map the data extent into the canvas, inside
-- the margins. y is flipped (SVG y points down: max paper-y → top margin).
-- STUB / TODO: preserve aspect (single uniform scale so atoms stay circular and
--   to scale Disc radii); fold pan (taux/tauy) and interactive zoom in here.
xScale :: (Double, Double) -> Double -> Double
xScale dom = coordVal . linearScale (Domain dom)
                                    (Range (Length left_margin, Length (width - right_margin)))

yScale :: (Double, Double) -> Double -> Double
yScale dom = coordVal . linearScale (Domain dom)
                                    (Range (Length (height - bottom_margin), Length top_margin))

-- bounding box of all primitive coords (minX,maxX,minY,maxY); Nothing if empty
pictureExtent :: Picture -> Maybe (Double, Double, Double, Double)
pictureExtent (Picture prims) = case concatMap coords prims of
    [] -> Nothing
    ps -> Just ( minimum (map vx ps), maximum (map vx ps)
               , minimum (map vy ps), maximum (map vy ps) )
  where
    vx (V2 x _) = x ; vy (V2 _ y) = y
    coords (Disc cx cy _ _) = [V2 cx cy]
    coords (Polyline p _)   = p
    coords (Polygon  p _)   = p

-- | SVG interpreter (d3x/hsx). Other backends (blank-canvas, eps, …) are just
--   more interpreters over the same Picture.
renderSvg :: Picture -> Html
renderSvg pic@(Picture prims) = foldMap prim prims
  where
    (sx, sy) = case pictureExtent pic of
                 Just (mnx, mxx, mny, mxy) -> (xScale (mnx, mxx), yScale (mny, mxy))
                 Nothing                   -> (id, id)
    pt (V2 x y) = V2 (sx x) (sy y)
    prim (Disc cx cy r fill)   = [hsx|<circle cx={tshow (sx cx)} cy={tshow (sy cy)} r={tshow r} fill={fill} stroke="black"/>|]
    prim (Polyline pts stroke) = [hsx|<path d={d3Line (map pt pts)} fill="none" stroke={stroke}/>|]
    prim (Polygon  pts fill)   = [hsx|<path d={d3Line (map pt pts) <> "Z"} fill={fill} stroke="black"/>|]

