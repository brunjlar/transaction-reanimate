{-# LANGUAGE OverloadedStrings #-}

module Transaction where

import Data.Text        (Text, pack)
import Graphics.SvgTree hiding (Text)
import Linear.V2
import Reanimate

test :: IO ()
test = reanimate $ a0 `andThen` (a1 `seqA` a2 `seqA` a3 `seqA` a4 `seqA` a5 `seqA` a6)

scenario :: Text -> Text -> Animation
scenario x y =
    ( output x       "100 ₳" y  (-10, 1) (-6, 3) `parA`
      output "Bob"    "50 ₳" "" (-10, 0) (-4, -4)
    )                                                   `andThen`
    tx 1 (-3, 1)                                        `andThen`
    input "" (-3, 1) (-6, 3)                            `andThen`
    output "Bob"   "10 ₳" "" (-3, 0.5) (0, -2)          `andThen`
    output "Alice" "90 ₳" "" (-3, 1.5) (-0.5, 2)        `andThen`
    tx 2 (2, -2)                                        `andThen`
    input "" (2, -1.5) (-0.5, 2)                        `andThen`
    input "" (2, -2) (0, -2)                            `andThen`
    input "" (2, -2.5) (-4, -4)                         `andThen`
    output "Alice"    "35 ₳" "" (2, -1.5) (6, 3.5)      `andThen`
    output "Bob"       "5 ₳" "" (2, -2)   (7, 1.5)      `andThen`
    output "Charlie" "110 ₳" "" (2, -2.5) (6.5, -4)     `andThen`
    pause 1

a0, a1, a2, a3, a4, a5, a6 :: Animation
a0 = staticFrame 0 $ mkBackground "white"
a1 = scenario "Alice" ""

a2 = animate (\t -> scale (1 + 2.1 * t * t) $ translate (6 * t) (-3 * t) svg) `andThen` pause 1
  where
    svg = lastFrame a1

a3 = lastFrame a2 `fade`scale 3.1 (translate 6 (-3) $ lastFrame $ scenario "Script" "") `andThen` pause 1

a4 = animate (\t -> scale (3.1 - 2.1 * t) $ translate ((-6) * t) (3 * t) svg) `andThen` pause 1
  where
    svg = scale (1 / 3.1) $ lastFrame a3

a5 = animate (\t -> scale (1 + 0.55 * t) $ translate (3.6 * t) (- 0.6 * t) svg) `andThen` pause 1
  where
    svg = lastFrame a4

a6 = lastFrame a5 `fade`scale 1.55 (translate 3.6 (-0.6) $ lastFrame $ scenario "Script" "Datum") `andThen` pause 1

lastFrame :: Animation -> Tree
lastFrame a = frameAt (duration a) a

fade :: Tree -> Tree -> Animation
fade a b = applyE fadeOutE (staticFrame 1 a) `parA` applyE fadeInE (staticFrame 1 b)

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
    setDuration 0.4  a                      `andThen`
    setDuration 0.15 b                      `andThen`
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
                [ MoveTo OriginAbsolute [V2 (x1 + 0.35) y1]
                , CurveTo OriginAbsolute [(V2 (x1 + 1.35) y1, V2 (x2 - 2) y2, V2 (x2 - 1) y2)]
                , LineTo OriginAbsolute [V2 (x2 - 0.1) y2]
                ]
    b = animate $ \s -> translate x2 y2 $ partialSvg s $ pathify outputPort

    t :: Tree -> Animation
    t = staticFrame 0.15 . translate x2 y2

tx :: Int -> (Double, Double) -> Animation
tx i (x, y) = setDuration 0.2 a `andThen` staticFrame 0.8 t
  where
    a :: Animation
    a = animate $ \s -> partialSvg s $ pathify
            $ withStrokeColor "black"
            $ withFillColor "gray"
            $ translate x y
            $ mkRect 0.7 2

    t :: Tree
    t = translate x (y - 0.05) $ text $ pack $ "Tx " ++ show i

input :: Text -> (Double, Double) -> (Double, Double) -> Animation
input red (x1, y1) (x2, y2) =
    setDuration 0.6 a `andThen`
    setDuration 0.2 b `andThen`
    staticFrame 0.2 c
  where
    a, b :: Animation
    a =  animate $ \s ->
            partialSvg s
          $ withStrokeWidth 0.03
          $ withStrokeColor "blue"
          $ withFillOpacity 0
          $ mkPath
                [ MoveTo OriginAbsolute [V2 (x1 - 0.35) y1]
                , CurveTo OriginAbsolute [(V2 (x1 - 1.15) y1, V2 (x2 + 2) y2, V2 (x2 + 1) y2)]
                , LineTo OriginAbsolute [V2 x2 y2]
                ]

    b = animate $ \s ->
              withFillColor "blue"
            $ withStrokeWidth 0
            $ translate x2 y2
            $ partialSvg s
            $ pathify
            $ mkCircle 0.05

    c :: Tree
    c = translate (x2 + 0.5) (y2 + 0.1) $ text red
