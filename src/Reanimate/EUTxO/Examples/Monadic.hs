{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

module Reanimate.EUTxO.Examples.Monadic
    ( monadic
    ) where

import Control.Monad                  (void)

import Reanimate.EUTxO.Monadic.Simple

monadic :: IO ()
monadic = reanimateM example

example :: AnimationM ()
example = do
    txGen   <- txM False (-10, 0)
    oAlice1 <- outputM "Alice" Nothing "100 ada" txGen (-5, 0)
    delayM
    tx <- txM True (0, 0)
    inputM Nothing oAlice1 tx
    void $ outputM "Bob" Nothing "10 ada" tx (3, 2)
    void $ outputM "Alice" Nothing "90 ada" tx (4, -1)
    delayM

