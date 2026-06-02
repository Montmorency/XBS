module D3X.XBS.XBS where

import D3X.Prelude
import D3X.Blocks
import D3X.Scales

import Linear
import Linear.Quaternion (rotate)

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VAM  -- stable merge sort

import           Data.Ord     (comparing)
import           Control.Lens ((^.))
import           Linear.V3    (V3, _z)

-- XBS Top Types
type Perspective = Bool

data Config = Config { bline :: Bool
                     , dist0 :: Float
                     , scale :: Float
                     , tmat :: M33
                     }

data Ball = Ball { pos :: V3
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

data BotCoord = BotCoord Float Int IPVec FlagVec

type SpeciesName = Text
  
type MinLength = Float
type MaxLength = Float
type Radius    = Float
type Color     = Float                          

data Bond = Bond SpeciesName SpeciesName MinLength MaxLength Radius Color

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

init_tmat = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

height = 450
width = 450
left_margin = 20
right_margin = 20
top_margin = 20
bottom_margin = 20
dalfa = 0.08726646259971647
bndfac = 1
maxrad = 100

-- This is to accumulate the atomic ordering from bottom to top along
------- sort atoms back to front ----- 
sortByZ :: Ord a => V.Vector (V3 a) -> V.Vector (V3 a)
sortByZ = V.modify (VAM.sortBy (comparing (^. _z)))

phiVec i = let phi = i*pi/(npoints-1) in V2 (-1.0*(sin phi)) (cos phi))
arcs = phiVec <$> [0 .. npoints-1]

xScale minX maxX = linearScale Domain (minX, maxX) (Range leftMargin (width - rightMargin))
yScale minY maxY = linearScale Domain (minY, maxY) (Range bottomMargin (top - topMargin))
  
pcoords :: [(Atom, V3)] -> M33 -> [(Atom, V3)]
pcoords globCoords tmat = (tmat :: M33) ^*^ V3

d3Line :: [V2] -> SvgPath
d3Line = ... fill this in D3X.Path...
  
-- We need a d3Line function takes a list of points?  
plotBonds :: Bool -> (V3, [(V3, V3)])  -> Html
plotBonds bLine (atom, bonds) = if bLine
                                then mapM plotThinBond bonds
                                else mapM plotThickBond bonds
  

checkZero vec = if (abs(norm vec) < 0.0001)
                   then V3 1 0 0
                   else vec

line bonds = d3Line $ map (\atom -> V2 (xScale . _x atom) (yScale . _y atom) bonds

atomPos :: V3 -> Radius -> V3
atomPos locP rad scale =
  let                      
    q = locP - (V3 0 0 dist0)
    y = checkZero (locP |+| (((dot locP (locP - d)) / (norm q)) |* q))
    a = (rad^2)/(norm q)
    b = rad * (sqrt ((1+a) / (quadrance y)))
    
    V3 v1x v1y v1z = a *^ q ^+^ (b *^ y)
    V3 v2x v2y v2z = a *^ q ^-^ (b *^ y)
    
    za1 = (scale*v1x*dist0) / (dist0 - v1z)
    za2 = (scale*v1y*dist0) / (dist0 - v1z)
    
    zb1 = (scale*v2x*dist0) / (dist0 - v2z)
    zb2 = (scale*v2y*dist0) / (dist0 - v2z)
  in
    if perspective
    then let
            zr' = rad*dist0 / (dist0 - (locP ! 2))
            zr = if (((dist0 - (locP ! 2)) > 0) && (zr' < maxRad))
                  then zr'
                  else maxRad
         in ((V3 (locP ^. _x) (locP ^. _y) zr) |* scale)
    else V3 (0.5*(za1+zb2)) (0.5*(za2+zb2)) (0.5 * sqrt((ab1-za1)^2) + (zb2-za2)^2)
  
drawBallAndSticks config tmat ballAndSticks@(ball,sticks) = do
       let
         -- | rotate bonds and sort back to front on z
         sortedSticks = (sortOn (\stick -> stick ^. (.ball) . (.pos) . _z)
                         map (\stick -> (tmat ^*^ stick.ball.pos)) sticks)

        in map (plotBond config ball) sortedSticks

plotBond config ballk sortedStick = let
         ballkk = sortedStick.end
         
         b@(V3 bx by _)  = atompos(ballkk) ^-^ atompos(ballk)
         
         xx = sqrt(bx^2 + by^2)
         -- continue if xx*xx < 0.0001
         -- if (xx^2 < 0.0001)
         -- then mempty
         --  else 
         q1 = d ^-^ ballk.pos
         q2 = d ^-^ ballkk.pos
         b = ballkk.pos ^-^ ballk.pos

         th1 = acos (((dot q1 b))/(sqrt(quadrance q1 *  quadrance b)))
         th2 = acos ((dot q2 b)/(sqrt(quadrance q2 * quadrance b)))

         crit1 = let tmp = asin (br/rk) * fudgefac
                 in if (tmp < 0.0) then 0.0 else tmp
                                                 
         crit2 = let tmp = asin (br/rkk) * fudgefac
                 in if (tmp < 0.0) then 0.0 else tmp
         
         note = setNote th2 th1 crit1 crit2 k kk
         
         when (note == 1 | note == 2) do
            let  
            -- | these aren't necessarily anything to do with plucker
            --   coords but it gives a convenient 6 element vector
              m1@(Plucker m10 m11 m12 m13 m14 m15) = calcM ballk th1
              m2@(Plucker m20 m21 m22 m23 m24 m25) = calcM ballkk th2
              
              beta = exp (gslope*(0.5*(k_z + kk_z)-gz0)*gslope)

              if bline
                then d3Line [(V2 m14 m15), (V2 m24 m25)] -- Straight thin line
                else d3Line
                     (map (\(V2 arcX arcY) ->
                            let x =  m10*arcX + m12*arcY + m14
                                y =  m11*arcX + m13*arcY + m15
                            in V2 x y
                         ) arcs
                     ++
                     (map (\(V2 arcX arcY) ->
                            let x =  m20*arcX + m22*arcY + m24
                                y =  m21*arcX + m23*arcY + m25
                            in V2 x y
                         ) (reverse arcs))
                     )

calcM ball cth = let
              (x,y) = ((,) <$> _x <*> _y) atompos ball
              w = ball.rad^2 - br^2
              sth = sqrt (1.0-cth*cth);
              ww = w*sth*kz / rk
              bb = br*zk/rk
              aa = br*cth*zk/rk
        in Plucker (bx*aa) (by*aa) (-by*bb) (bx*bb) (kx + bx*ww + taux) (ky + by*ww + tauy)

-- | looks like we are trying to sort out the quadrant of acos/asin?
setNote th2 th1 crit1 crit2 k kk | ((th2-0.5*Math.PI > crit2) && (k<kk)) = 1
                                 | ((th1-0.5*Math.PI < crit1) && (k<kk)) = 2
                                 | otherwise = 0
tauy, taux = 0.0
fudgefac=0.6;
gslope = 0;
 
type BondMap =  M.Map ((Text,Text), Bond)
stick :: BondMap -> [Ball] -> [(Ball, [Stick])]
stick bondMap (ball1 : balls) = (ball1, catMaybes (map (\ball2 -> case M.lookup (ball1.species, ball2.species) bondMap of
                                             Nothing -> Nothing
                                             Just bond -> let
                                                 dis = norm (ball1.pos ^-^ ball2.pos)
                                               in
                                                 if ((dis >= bond.minLength) && dis <= bond.maxLength)
                                                 then Just (Stick ball1 ball2 bond.radius bond.col)
                                                 else Nothing) balls)) : (stick bondMap balls)

drawBalls3 :: Config -> V.Vector (Ball, [Stick]) -> Html
drawBalls3 config ballsAndSticks = 
  V.foldM (\plot (ball, sticks) -> do
                 plot
                 plotAtom ball
                 forEach sticks plotBonds 
                ) mempty (sortByZ ballsAndSticks)    

-- | Print target could be swapped e.g. change directives for
--   canvas, eps, tikz etc ...
  
plotThinBond = [hsx| <path stroke="black" fill="black" d={d3Line } |]
plotThickBond =  [hsx| <path stroke="black" fill="black" d={thickLine b} |]
    
plotAtom (atomCoord, radius) atomAttrs = [hsx| <circle 
                                                 cx={(xScale . _x . atomPos) atomCoord}
                                                 cy={(yScale . _y . atomPos)  atomCoord}
                                                 r={2*r}
                                                 fill={fill}
                                                 stroke="black"
                                                 />
                                             |]
  
