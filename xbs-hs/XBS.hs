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
import IHP.HSX.Markup (Html, renderMarkupText)

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
                     , wire :: Bool                 -- wireframe: atoms hollow (fill none), bonds only
                     , perspective :: Perspective   -- render mode (C pmode); toggled live
                     , dist0 :: Float
                     , scale :: Float
                     , zoom :: Double               -- interactive zoom multiplier (1 = fit-to-view)
                     , frame :: Int                 -- movie frame index (0 when static / no .mv)
                     , tmat :: Mat3
                     }

data Ball = Ball { idx :: Int          -- stable atom index (load order); identifies the atom for selection/focus
                 , pos :: Vec3
                 , rad :: Radius
                 , gray :: Color
                 , rgb :: RGB
                 , col :: Int
                 , special :: Int
                 , species :: Text
                 }

data Stick = Stick { start :: Ball
                   , end :: Ball
                   , rad :: Radius
                   , gray :: Float
                   , col :: Int
                   , rgb :: RGB
                   }


type SpeciesName = Text
  
type MinLength = Double   -- physics: compared against model-space bond lengths (Double)
type MaxLength = Double   -- physics: ditto
type Radius    = Double   -- physics-adjacent: feeds the projection geometry
type Color     = Float    -- presentation: a single gray value 0..1

-- | An RGB colour, each channel 0..1 (the .bs spec/bonds "colour" field; a
--   single gray g is read as RGB g g g).
data RGB = RGB !Double !Double !Double deriving (Eq, Show)

-- | RGB → a CSS colour string for SVG fill/stroke.
rgbToCss :: RGB -> Text
rgbToCss (RGB r g b) = "rgb(" <> ch r <> "," <> ch g <> "," <> ch b <> ")"
  where ch x = tshow (max 0 (min 255 (round (x * 255) :: Int)))

data Bond = Bond { sp1       :: SpeciesName
                 , sp2       :: SpeciesName
                 , minLength :: MinLength
                 , maxLength :: MaxLength
                 , radius    :: Radius
                 , rgb       :: RGB        -- the .bs "color" field (1 gray or 3 RGB values)
                 }

-- Backend-neutral drawing IR. The geometry fold produces a Picture (a free
-- monoid of primitives); each backend has its own interpreter (renderSvg via
-- d3x, renderCanvas via blank-canvas, ...). Nothing SVG/canvas-specific here.
data Prim = Disc     Int Double Double Double Text  -- ^ atomIdx cx cy r fill (atom; idx for selection)
          | Polyline [V2 Double] Text               -- ^ pts      stroke (thin bond)
          | Polygon  [V2 Double] Text               -- ^ pts      fill   (thick bond)
          deriving (Eq, Show)

newtype Picture = Picture [Prim]
  deriving (Eq, Show, Semigroup, Monoid)

defConfig = Config { bline = False   -- C default (pmode): thick cylinders, not lines
                   , wire = False
                   , perspective = False
                   , dist0 = 250
                   , scale = 15
                   , zoom = 1.0
                   , frame = 0
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
-- render globals (C uses these as globals; scale is C `fac`)
maxRad = 100                -- C MAXRAD: cap on projected radius
scale = 15                  -- C `fac`: overall plot scale (perspective now lives in Config)


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
    -- bonds first, then the atom on top: the atom covers its own near-caps,
    -- while the bond shafts (outside the atom radius) stay visible.
    foldMap drawOneBond sortedSticks <> plotAtom config.wire config.perspective (rot ball)
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
         V3 bdx bdy _ = atomPos config.perspective ballkk.pos ballkk.rad
                    ^-^ atomPos config.perspective ballk.pos  ballk.rad
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
         Plucker m10 m11 m12 m13 m14 m15 = calcM config.perspective KEnd  bx by br ballk  cth1
         Plucker m20 m21 m22 m23 m24 m25 = calcM config.perspective KKEnd bx by br ballkk cth2
  in if xx*xx < 0.0001            -- atoms project to ~the same point: skip
       then mempty
       else if config.bline
              then Picture [Polyline [V2 m14 m15, V2 m24 m25] (rgbToCss sortedStick.rgb)] -- thin line
              else Picture [Polygon                                    -- thick tapered cap outline
                   ( map (\(V2 arcX arcY) -> V2 (m10*arcX + m12*arcY + m14)
                                                (m11*arcX + m13*arcY + m15)) arcs
                  ++ map (\(V2 arcX arcY) -> V2 (-m20*arcX + m22*arcY + m24)  -- C/JS: -m2[0],-m2[1]
                                                (-m21*arcX + m23*arcY + m25)) (reverse arcs)
                   ) (rgbToCss sortedStick.rgb)]

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
                                                                  , gray = 0.5, col = 0  -- gray/col unused; colour via rgb
                                                                  , rgb = bond.rgb })
                                                 else Nothing) balls)) : (stick bondMap balls)

-- | Whole scene → Picture: z-order atoms back to front, then foldMap the core
--   step over them. Backend-neutral; pick an interpreter (renderSvg/…) after.
drawScene :: Config -> Mat3 -> V.Vector (Ball, [Stick]) -> Picture
drawScene config tmat = foldMap (drawBallAndSticks config tmat) . sortByZ tmat

-- | One atom as a Disc primitive (ball is expected already in view space).
--   In wireframe mode the disc is hollow (fill "none") so only its outline and
--   the bonds show — mirrors the JS `wire_fill` (atom fill = none when wire).
plotAtom :: Bool -> Perspective -> Ball -> Picture
plotAtom wire persp ball =
  let V3 px py pr = atomPos persp ball.pos ball.rad   -- paper x,y + projected radius
      fill = if wire then "none" else rgbToCss ball.rgb
  in Picture [Disc ball.idx (px + taux) (py + tauy) pr fill]

-- A 2D viewport transform: maps x, y and a length/radius (each Double->Double).
-- Local for now; may promote to a d3x multi-dimensional scale type later, driven
-- by downstream backends.
type Transform2D = (Double -> Double, Double -> Double, Double -> Double)

-- | One uniform paper→viewport scale, built on d3x's LinearScale (Domain→Range).
--   The SAME factor maps x, y AND r (so atoms stay round and flush with bonds —
--   cf. C hardcopy: balls+sticks share one space, radius used directly). The
--   data extent (x,y Domains) is fitted inside the margins, centred, y flipped
--   (SVG y points down). Returns (mapX, mapY, mapR).
--   @zoom@ multiplies the fit-to-view factor about the data centre (zoom 1 =
--   fits the margins; >1 magnifies, may overflow the viewBox — that's intended).
--   TODO: fold pan (taux/tauy) in here too.
viewportScale :: Double -> Domain Double -> Domain Double -> Transform2D
viewportScale zoom (Domain (mnx, mxx)) (Domain (mny, mxy)) =
    ( \x -> width  / 2 + s (x - cx0)
    , \y -> height / 2 - s (y - cy0)        -- flip y
    , s )
  where
    maxSpan = max 1e-9 (max (mxx - mnx) (mxy - mny))
    avail   = min (width - left_margin - right_margin)
                  (height - top_margin - bottom_margin)
    -- one factor via d3x: map a delta in [0,maxSpan] → [0,avail], then zoom
    s d     = zoom * coordVal (linearScale (Domain (0, maxSpan)) (Range (Length 0, Length avail)) d)
    cx0     = 0.5 * (mnx + mxx)
    cy0     = 0.5 * (mny + mxy)

-- the data extent as x and y Domains (Nothing if the Picture is empty)
pictureExtent :: Picture -> Maybe (Domain Double, Domain Double)
pictureExtent (Picture prims) = case concatMap coords prims of
    [] -> Nothing
    ps -> Just ( Domain (minimum (map vx ps), maximum (map vx ps))
               , Domain (minimum (map vy ps), maximum (map vy ps)) )
  where
    vx (V2 x _) = x ; vy (V2 _ y) = y
    coords (Disc _ cx cy r _) = [V2 (cx-r) (cy-r), V2 (cx+r) (cy+r)]  -- include the disc, not just its centre
    coords (Polyline p _)   = p
    coords (Polygon  p _)   = p

-- | SVG interpreter (d3x/hsx). Other backends (blank-canvas, eps, …) are just
--   more interpreters over the same Picture.
-- | Shared SVG interpreter: fits the Picture to the viewport (with @zoom@) and
--   renders bonds; how each atom disc is drawn is left to the @disc@ callback
--   (idx + already-scaled cx cy r + fill), so the static and live paths differ
--   only in the circle markup. Bonds are identical across paths.
renderWith :: (Int -> Double -> Double -> Double -> Text -> Html)
           -> Double -> Picture -> Html
renderWith disc zoom pic@(Picture prims) = foldMap prim prims
  where
    (sx, sy, sr) = case pictureExtent pic of
                     Just (dx, dy) -> viewportScale zoom dx dy
                     Nothing       -> (id, id, id)
    pt (V2 x y) = V2 (sx x) (sy y)
    prim (Disc i cx cy r fill) = disc i (sx cx) (sy cy) (sr r) fill
    prim (Polyline pts stroke) = [hsx|<path d={d3Line (map pt pts)} fill="none" stroke={stroke}/>|]
    prim (Polygon  pts fill)   = [hsx|<path d={d3Line (map pt pts) <> "Z"} fill={fill} stroke="black"/>|]

-- | Static interpreter (xbs-render): plain circles, no interaction markup.
renderSvg :: Picture -> Html
renderSvg = renderWith staticDisc 1.0
  where
    staticDisc _ cx cy r fill =
      [hsx|<circle cx={tshow cx} cy={tshow cy} r={tshow r} fill={fill} stroke="black"/>|]

-- | Live interpreter (xbs-live): each atom is a clickable htmx target
--   (@GET /focus?atom=i@, shift-click via @hx-vals@), and atoms in the @focus@
--   stack are highlighted. @zoom@ is the interactive zoom multiplier.
renderSvgLive :: [Int] -> Double -> Picture -> Html
renderSvgLive focus = renderWith liveDisc
  where
    liveDisc i cx cy r fill =
      let foc    = i `elem` focus
          stroke = if foc then "#d00000" else "black"     :: Text
          sw     = if foc then "3"       else "1"         :: Text
      in [hsx|<circle
                hx-get={"/focus?atom=" <> tshow i}
                hx-vals="js:{multi: event.shiftKey ? 1 : 0}"
                hx-target="#data" hx-swap="innerHTML"
                style="cursor:pointer"
                cx={tshow cx} cy={tshow cy} r={tshow r}
                fill={fill} stroke={stroke} stroke-width={sw}/>|]


------------------------------------------------------------------------
-- Live view control (used by the xbs-live interactive viewer)
------------------------------------------------------------------------

-- | Rotate a view matrix by @alfa@ radians about screen axis @ixyz@ and
--   pre-multiply (newTmat = rot * tmat). Faithful port of C rotmat (subs.h 846);
--   note the C axis convention: 1 = screen-y (left/right), 2 = screen-x
--   (up/down), 3 = in-plane (ccw/cw).
rotmat :: Int -> Double -> Mat3 -> Mat3
rotmat ixyz alfa t = case ixyz of
    1 -> V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c) !*! t
    2 -> V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c) !*! t
    3 -> V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1) !*! t
    _ -> t
  where c = cos alfa; s = sin alfa

-- | A view/render command from the interactive driver.
data Cmd = RotL | RotR | RotU | RotD | RotCCW | RotCW   -- arrows / ' / / / < / >
         | TogglePersp | ToggleLine | ToggleWire         -- p / l / w
         | ZoomIn | ZoomOut                               -- + (=) / -
         | ResetView                                      -- r
         | FramePrev | FrameNext | FrameFirst | FrameLast -- [ ] j k
         | Quit | NoOp
  deriving (Eq, Show)

-- | Per-keypress zoom factor (multiplicative).
zoomStep :: Double
zoomStep = 1.1

-- | Apply a command to the Config. @home@ is the original view (for ResetView);
--   @nframes@ is the movie length (1 when static) so frame steps wrap modularly,
--   matching the JS @advance_frame@ (index + n `mod` nframes).
applyCmd :: Mat3 -> Int -> Cmd -> Config -> Config
applyCmd home nframes cmd cfg = case cmd of
    RotL        -> rot 1 (-dalfa)
    RotR        -> rot 1   dalfa
    RotU        -> rot 2 (-dalfa)
    RotD        -> rot 2   dalfa
    RotCCW      -> rot 3   dalfa
    RotCW       -> rot 3 (-dalfa)
    TogglePersp -> cfg { perspective = not cfg.perspective }
    ToggleLine  -> cfg { bline = not cfg.bline }
    ToggleWire  -> cfg { wire  = not cfg.wire }
    ZoomIn      -> cfg { zoom = cfg.zoom * zoomStep }
    ZoomOut     -> cfg { zoom = cfg.zoom / zoomStep }
    ResetView   -> cfg { tmat = home, zoom = 1.0 }
    FrameNext   -> cfg { frame = step   1 }
    FramePrev   -> cfg { frame = step (-1) }
    FrameFirst  -> cfg { frame = 0 }
    FrameLast   -> cfg { frame = max 0 (nframes - 1) }
    _           -> cfg                                  -- Quit/NoOp: driver handles
  where
    rot ax a = cfg { tmat = rotmat ax a cfg.tmat }
    step d | nframes <= 1 = cfg.frame
           | otherwise    = (cfg.frame + d) `mod` nframes

-- | Render the scene for the current Config to a standalone SVG document
--   (one render per interactive frame). @focus@ is the current selection stack
--   (atoms drawn highlighted + made clickable). Bonds are precomputed and passed in.
renderConfigSvg :: [Int] -> Config -> V.Vector (Ball, [Stick]) -> Text
renderConfigSvg focus cfg ballsAndSticks =
  renderMarkupText
    (svgViewBox (round width) (round height)
                (renderSvgLive focus cfg.zoom (drawScene cfg cfg.tmat ballsAndSticks)))

-- | The data pane (htmx swap target @#data@): one card per focused atom, newest
--   first. v1 shows index + species + current-frame coords; LDOS / bonding /
--   forces are stubbed pending the data source (duckdb / H-params, see CLAUDE.md).
--   Returns an HTML fragment as Text (what the @/focus@ handler responds with).
focusPanel :: V.Vector Ball -> [Int] -> Text
focusPanel balls focus = renderMarkupText [hsx|
  <div>
    <div style="font:600 13px sans-serif;margin-bottom:8px">focus stack ({tshow (length focus)})</div>
    {body}
  </div>|]
  where
    body :: Html
    body | null focus = [hsx|<div style="color:#888;font:12px sans-serif">click an atom · shift-click to add</div>|]
         | otherwise  = foldMap atomCard focus
    byIdx i = V.find (\b -> b.idx == i) balls
    atomCard i = case byIdx i of
      Nothing -> mempty
      Just b  ->
        let V3 x y z = b.pos
            num v = tshow (fromIntegral (round (v * 1000) :: Int) / 1000 :: Double)
        in [hsx|
          <div style="border:1px solid #ddd;border-radius:6px;padding:6px 8px;margin-bottom:6px;font:12px ui-monospace,monospace">
            <div style="font-weight:600">#{tshow i} · {b.species}</div>
            <div style="color:#555">x {num x}  y {num y}  z {num z}</div>
            <div style="color:#aaa;font-size:11px;margin-top:3px">LDOS / bonding / forces — pending</div>
          </div>|]
