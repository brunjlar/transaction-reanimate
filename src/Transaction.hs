{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Data.Text        (Text)
import Graphics.SvgTree hiding (Text)
import Linear.V2
import Reanimate
import Reanimate.Raster

test :: IO ()
test = reanimate $ animate $ \t -> mkGroup
    [ mkBackground "white"
    , frameAt t $ output "Alice" "100 ₳" "Datum" (-3, 2) (1, -1)
    ]

outputPort :: Tree
outputPort = withStrokeWidth 0.03
           $ withStrokeColor "red"
           $ withFillOpacity 0
           $ mkCircle 0.085

text :: Text -> Tree
text = scale 0.1
     . withFillColor "black"
     . withStrokeColor "black"
     . mkText

output :: Text -> Text -> Text -> (Double, Double) -> (Double, Double) -> Animation
output addr amt dat (x1, y1) (x2, y2) =
    setDuration 0.4  a                       `andThen`
    setDuration 0.15 b                       `andThen`
    t (translate (-0.5)   0.1  $ text addr) `andThen`
    t (translate (-0.5) (-0.2) $ text amt)  `andThen`
    t (translate (-0.5) (-0.4) $ text dat)
  where
    a, b :: Animation
    a = animate $ \s ->
            partialSvg s
          $ withStrokeWidth 0.03
          $ withStrokeColor "red"
          $ withFillOpacity 0
          $ mkPath
                [ MoveTo OriginAbsolute [V2 x1 y1]
                , CurveTo OriginAbsolute [(V2 (x1 + 1) y1, V2 (x2 - 2) y2, V2 (x2 - 1) y2)]
                , LineTo OriginAbsolute [V2 (x2 - 0.1) y2]
                ]
    b = animate $ \s -> translate x2 y2 $ partialSvg s $ pathify outputPort

    t :: Tree -> Animation
    t = staticFrame 0.15 . translate x2 y2
