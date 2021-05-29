{-# LANGUAGE OverloadedStrings #-}

module Reanimate.EUTxO.Examples.Transaction
    ( transactions
    ) where

import Reanimate.EUTxO.Core

transactions :: IO ()
transactions = reanimate $ a0 `andThen` (a1 `seqA` a2 `seqA` a3 `seqA` a4 `seqA` a5 `seqA` a6)

scenario :: Text -> Text -> Text -> Animation
scenario x y z =
    ( output x       "100 ada" y  (-10, 1) (-6, 3) `parA`
      output "Bob"    "50 ada" "" (-10, 0) (-4, -4)
    )                                                     `andThen`
    tx 1 (-3, 1)                                          `andThen`
    input z (-3, 1) (-6, 3)                               `andThen`
    output "Bob"   "10 ada" "" (-3, 0.5) (0, -2)          `andThen`
    output "Alice" "90 ada" "" (-3, 1.5) (-0.5, 2)        `andThen`
    tx 2 (2, -2)                                          `andThen`
    input "" (2, -1.5) (-0.5, 2)                          `andThen`
    input "" (2, -2) (0, -2)                              `andThen`
    input "" (2, -2.5) (-4, -4)                           `andThen`
    output "Alice"    "35 ada" "" (2, -1.5) (6, 3.5)      `andThen`
    output "Bob"       "5 ada" "" (2, -2)   (7, 1.5)      `andThen`
    output "Charlie" "110 ada" "" (2, -2.5) (6.5, -4)     `andThen`
    pause 1

a0, a1, a2, a3, a4, a5, a6 :: Animation
a0 = staticFrame 0 $ mkGroup
        [ mkImage 16 9 "Cardano.jpg" -- mkBackground "white"
        , withFillOpacity 0.8 $ mkBackground "white"
        ]

a1 = scenario "Alice" "" ""

a2 = animate (\t -> scale (1 + 2.1 * t * t) $ translate (6 * t) (-3 * t) svg) `andThen` pause 1
  where
    svg = lastFrame a1

a3 = lastFrame a2 `fade`scale 3.1 (translate 6 (-3) $ lastFrame $ scenario "Script" "" "Redeemer") `andThen` pause 1

a4 = animate (\t -> scale (3.1 - 2.1 * t) $ translate ((-6) * t) (3 * t) svg) `andThen` pause 1
  where
    svg = scale (1 / 3.1) $ lastFrame a3

a5 = animate (\t -> scale (1 + 0.55 * t) $ translate (3.6 * t) (- 0.6 * t) svg) `andThen` pause 1
  where
    svg = lastFrame a4

a6 = lastFrame a5 `fade`scale 1.55 (translate 3.6 (-0.6) $ lastFrame $ scenario "Script" "Datum" "Redeemer") `andThen` pause 1
