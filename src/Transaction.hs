module Transaction where

import Graphics.SvgTree
import Linear.V2
import Reanimate
import Reanimate.Raster

test :: IO ()
test = reanimate $ staticFrame 0.1 $ txOutput 0.07 0.1

txOutput :: Double -> Double -> Tree
txOutput r1 r2 = withStrokeWidth (r2 - r1)
               $ withStrokeColor "red"
               $ withFillOpacity 0
               $ mkCircle $ (r1 + r2) / 2
