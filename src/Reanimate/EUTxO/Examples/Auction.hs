{-# LANGUAGE RebindableSyntax           #-}

module Reanimate.EUTxO.Examples.Auction
    ( auction
    ) where

import Reanimate

import Reanimate.EUTxO.Monadic.Advanced

auction :: IO ()
auction = reanimate $ auction1 `seqA` auction2

auction1 :: Animation
auction1 = andThen background $ runAnimationM $ do
    tx1        <- txM False (-11, 0)
    oAuction1  <- outputM "Auction" "NFT" (Just "Nothing") tx1 (-7, 2)
    delayM
    tx2        <- txM False (-11, -3)
    oBob1      <- outputM "Bob" "100 ada" Nothing tx2 (-6, -2)
    delayM
    tx3        <- txM True (-4, 0)
    inputM (Just "bid") oAuction1 tx3
    inputM Nothing oBob1 tx3
    oAuction2  <- outputM "Auction" "NFT + 100 ada" (Just "(Bob, 100)") tx3 (-2, 1)
    delayM
    tx4        <- txM False (-11, 7)
    oCharlie1  <- outputM "Charlie" "200 ada" Nothing tx4 (-3, 4)
    delayM
    tx5        <- txM True (0, 1)
    inputM Nothing oCharlie1 tx5
    inputM (Just "bid") oAuction2 tx5
    oAuction3  <- outputM "Auction" "NFT + 200 ada" (Just "(Charlie, 200)") tx5 (3, 3)
    _oBob2     <- outputM "Bob" "100 ada" Nothing tx5 (3, -4)
    delayM
    tx6        <- txM True (5, 1)
    inputM (Just "close") oAuction3 tx6
    delayM
    _oCharlie2 <- outputM "Charlie" "NFT" Nothing tx6 (7, 2)
    _oAlice    <- outputM "Alice" "200 ada" Nothing tx6 (7.5, 0)
    delayM

auction2 :: Animation
auction2 = andThen background $ runAnimationM $ do
    tx1      <- txM False (-11, 0)
    oAuction <- outputM "Auction" "NFT" (Just "Nothing") tx1 (-5, 2)
    delayM
    tx2      <- txM True (-2, 0)
    inputM (Just "close") oAuction tx2
    _oAlice  <- outputM "Alice" "NFT" Nothing tx2 (2, 1)
    delayM

background :: Animation
background = staticFrame 0 $ mkGroup
    [ mkImage 16 9 "auction.png"
    , withFillOpacity 0.8 $ mkBackground "white"
    ]
