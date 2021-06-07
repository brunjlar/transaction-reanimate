{-# LANGUAGE RebindableSyntax           #-}

module Reanimate.EUTxO.Examples.Uniswap
    ( uniswap
    ) where

import Reanimate

import Reanimate.EUTxO.Monadic.Advanced

uniswap :: IO ()
uniswap = reanimate $ andThen background $ runAnimationM $ do
    tx1       <- txM True (-7.5, 0)
    oFactory1 <- outputM "Factory" "US NFT" (Just "[]") tx1 (-6, 3)
    delayM
    tx2       <- txM False (-10, 3)
    oAlice1   <- outputM "Alice" "1000A+2000B" Nothing tx2 (-5, 4)
    delayM
    tx3       <- txM True (-3.2, 3)
    inputM Nothing oAlice1 tx3
    inputM (Just "Create") oFactory1 tx3
    oPool1    <- outputM "Pool AB" "1000A+2000B+AB NFT" (Just "1415") tx3 (0, 2.5)
    oFactory2 <- outputM "Factory" "US NFT" (Just "[AB]") tx3 (0, 1)
    oAlice2   <- outputM "Alice" "1415AB" Nothing tx3 (-1, 0)
    delayM
    tx4       <- txM False (-2, 6)
    oBob1     <- outputM "Bob" "100A" Nothing tx4 (1.5, 3)
    tx5       <- txM True (3.5, 2)
    inputM Nothing oBob1 tx5
    inputM (Just "Swap") oPool1 tx5
    oBob2     <- outputM "Bob" "181B" Nothing tx5 (5, 4)
    oPool2    <- outputM' 4 "Pool AB" "1100A+1819B+AB NFT" (Just "1415") tx5 (-5.5, -3)
    delayM
    tx6       <- txM False (-9, -7)
    oCharlie1 <- outputM "Charlie" "400A+800B" Nothing tx6 (-6.5, -4)
    tx7       <- txM True (-3.5, -3.3)
    inputM (Just "Add") oPool2 tx7
    inputM Nothing oCharlie1 tx7
    oPool3    <- outputM "Pool AB" "1500A+2619B+AB NFT" (Just "1982") tx7 (-1.8, -2)
    oCharlie2 <- outputM "Charlie" "567AB" Nothing tx7 (3, -4)
    delayM
    tx8       <- txM True (1, -2.5)
    inputM Nothing oAlice2 tx8
    inputM (Just "Remove") oPool3 tx8
    oAlice3   <- outputM "Alice" "1070A+1869B" Nothing tx8 (3, -1)
    oPool4    <- outputM "Pool AB" "430A+750B+AB NFT" (Just "567") tx8 (3, -2.5)
    delayM
    tx9       <- txM True (5.5, -1)
    inputM (Just "Close") oFactory2 tx9
    inputM (Just "Close") oPool4 tx9
    inputM Nothing oCharlie2 tx9
    oFactory3 <- outputM "Factory" "US NFT" (Just "[]") tx9 (7.8, 3)
    oCharlie3 <- outputM "Charlie" "430A+750B" Nothing tx9 (7.8, -3)
    delayM

background :: Animation
background = staticFrame 0 $ mkGroup
    [ mkImage 16 9 "stock-exchange.jpg"
    , withFillOpacity 0.8 $ mkBackground "white"
    ]
{-
    txGen   <- txM False (-10, 0)
    oAlice1 <- outputM "Alice" "100 ada" Nothing txGen (-5, 0)
    delayM
    tx <- txM True (0, 0)
    inputM Nothing oAlice1 tx
    void $ outputM "Bob" "10 ada" Nothing tx (3, 2)
    void $ outputM "Alice" "90 ada" Nothing tx (4, -1)
    delayM
-}
