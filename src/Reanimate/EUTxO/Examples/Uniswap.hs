{-# LANGUAGE RebindableSyntax           #-}

module Reanimate.EUTxO.Examples.Uniswap
    ( uniswap
    ) where

import Control.Monad                    (void)

import Reanimate.EUTxO.Monadic.Advanced

uniswap :: IO ()
uniswap = reanimateM $ do
    txGen   <- txM False (-10, 0)
    oAlice1 <- outputM "Alice" "100 ada" Nothing txGen (-5, 0)
    delayM
    tx <- txM True (0, 0)
    inputM Nothing oAlice1 tx
    void $ outputM "Bob" "10 ada" Nothing tx (3, 2)
    void $ outputM "Alice" "90 ada" Nothing tx (4, -1)
    delayM
