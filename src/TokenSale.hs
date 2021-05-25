{-# LANGUAGE OverloadedStrings #-}

module TokenSale
    ( tokenSale
    ) where

import Core

tokenSale :: IO ()
tokenSale = reanimate $ a0 `andThen` a1

{-
scenario1 :: Text -> Animation
scenario1 x =
    output "Oracle" x  "1.75" (-10,  1) (-6,    0) `andThen`
    pause 1                                        `andThen`
    output "Oracle" "" "1.70" (-10,  5) (-6.5,  3) `andThen`
    output "Oracle" "" "1.85" (-10, -4) (-6.5, -2) `andThen`
    pause 1
-}

a0, a1 :: Animation

a0 = staticFrame 1 $ mkGroup
        [ mkImage 16 9 "Cardano.jpg"
        , withFillOpacity 0.8 $ mkBackground "white"
        ]

a1 =
    output "Seller" "NFT" "" (-10, 2.5) (-7, 2)         `andThen`
    pause 1                                             `andThen`
    tx 1 (-5, 2)                                        `andThen`
    input "" (-5, 2) (-7, 2)                            `andThen`
    output "TS" "NFT" "0" (-5, 2) (-3, 3)               `andThen`
    pause 1                                             `andThen`
    tx 2 (-1, 3)                                        `andThen`
    input "set price" (-1, 3) (-3, 3)                   `andThen`
    output "TS" "NFT" "6" (-1, 3) (1, 4)                `andThen`
    pause 1                                             `andThen`
    output "Seller" "5T" "" (-10, 0) (-1, 1)            `andThen`
    tx 3 (3, 2.5)                                       `andThen`
    input "add tokens" (3, 3) (1, 4)                    `andThen`
    input "" (3, 2) (-1, 1)                             `andThen`
    output' 6 "TS" "NFT+5T" "6" (3, 2.5) (-3, -3)       `andThen`
    pause 1                                             `andThen`
    output "Buyer" "12Ada" "" (-10, -2) (-4, -4)        `andThen`
    pause 1                                             `andThen`
    tx 4 (-1, -3)                                       `andThen`
    input "buy tokens" (-1, -2.5) (-3, -3)              `andThen`
    input "" (-1, -3.5) (-4, -4)                        `andThen`
    output "TS" "NFT+3T+12Ada" "6" (-1, -2.5) (1, -2.5) `andThen`
    output "Buyer" "2T" "" (-1, -3.5) (2, -4)           `andThen`
    pause 1                                             `andThen`
    tx 5 (4, -2)                                        `andThen`
    input "withdraw" (4, -2) (1, -2.5)                  `andThen`
    output "TS" "NFT+2T" "6" (4, -1.5) (6, 0)           `andThen`
    output "Seller" "1T+12Ada" "" (4, -2.5) (7, -4)

{-
a1 = scenario1 ""

a2 = (lastFrame a1 `fade` lastFrame (scenario1 "NFT")) `andThen` pause 1

a3 =
    pause 1                                                  `andThen`
    output "Swap"  "100 ada"         "" (-7, -7) (-5, -3)    `andThen`
    output "Buyer" "175 usd + 1 ada" "" (-6, -7) (-4, -4)    `andThen`
    tx 1 (-2, -1)                                            `andThen`
    pause 1                                                  `andThen`
    input "use" (-2, -0.5) (-6, 0)                           `andThen`
    output "Oracle" "NFT + 1 ada" "1.75" (-2, -0.5) (1, 2.5) `andThen`
    pause 1                                                  `andThen`
    input "swap" (-2, -1)   (-5, -3)                         `andThen`
    input ""     (-2, -1.5) (-4, -4)                         `andThen`
    output "Seller" "175 usd" "" (-2, -1)   (0.5,  0)        `andThen`
    output "Buyer"  "100 ada" "" (-2, -1.5) (1  , -3)        `andThen`
    pause 1                                                  `andThen`
    tx 2 (4, 1)                                              `andThen`
    input "update" (4, 1) (1, 2.5)                           `andThen`
    output "Oracle" "NFT"   "1.77" (4, 1.5) (7,    3)        `andThen`
    output "Owner"  "1 ada" ""     (4, 0.5) (7.4, -2)        `andThen`
    pause 1
-}
