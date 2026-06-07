xbs-hs flow:


  stick      :: BondMap -> [Ball] -> [(Ball,[Stick])]              -- build once
  drawScene  :: Config -> Mat3 -> V.Vector (Ball,[Stick]) -> Picture   -- z-sort atoms, foldMap
    └ drawBallAndSticks :: Config -> Mat3 -> (Ball,[Stick]) -> Picture -- rotate (view space), z-sort bonds
        ├ plotAtom :: Ball -> Picture                  -- Disc
        └ plotBond :: Config -> Ball -> Stick -> Picture  -- Polyline (thin) / Polygon (thick)
  renderSvg  :: Picture -> Html                        -- SVG interpreter (d3x/hsx)
  --  renderCanvas :: Picture -> Canvas ()             -- future: blank-canvas interpreter