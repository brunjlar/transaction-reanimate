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
    oAlice2   <- outputM "Alice" "1414AB" Nothing tx3 (-1, 2)
    oFactory2 <- outputM "Factory" "US NFT" (Just "[AB]") tx3 (-2, 0)
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
