{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

module Reanimate.EUTxO.Examples.Monadic
    ( monadic
    ) where

import Control.Monad                    (void)

import Reanimate.EUTxO.Monadic.Advanced

monadic :: IO ()
monadic = reanimateM $ do
    txGen   <- txM False (-10, 0)
    oAlice1 <- outputM "Alice" Nothing "100 ada" txGen (-5, 0)
    delayM
    tx <- txM True (0, 0)
    inputM Nothing oAlice1 tx
    void $ outputM "Bob" Nothing "10 ada" tx (3, 2)
    void $ outputM "Alice" Nothing "90 ada" tx (4, -1)
    delayM
