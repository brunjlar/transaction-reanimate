{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reanimate.EUTxO.Monadic.Advanced
    ( module Prelude
    , Nat (..)
    , LT
    , SNat (..)
    , toInt
    , KnownNat (..)
    , Output
    , Tx (..)
    , CheckTx
    , AnimationM
    , return, (>>=), (>>)
    , outputM
    , outputM'
    , txM
    , inputM
    , delayM
    , runAnimationM
    , reanimateM
    ) where

import           Control.Monad                  (void)
import           Data.Kind                      (Constraint, Type)
import           GHC.TypeLits                   (TypeError, ErrorMessage (..))
import           Prelude                        hiding (return, (>>=), (>>))
import qualified Prelude                        as P
import           Reanimate                      (Animation, reanimate)

import           Reanimate.EUTxO.Monadic.Simple (Pos)
import qualified Reanimate.EUTxO.Monadic.Simple as MS

data Nat = Z | S Nat

type family LT (m :: Nat) (n :: Nat) :: Constraint where
    LT 'Z     ('S n) = ()
    LT ('S m) ('S n) = LT m n
    LT _      _      = TypeError ('Text "tx trying to consume newer output")

type SNat :: Nat -> Type
data SNat n where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)

toInt :: SNat n -> Int
toInt SZ = 0
toInt (SS n) = succ $ toInt n

class KnownNat (n :: Nat) where
    theOne :: SNat n

instance KnownNat 'Z where
    theOne = SZ

instance KnownNat n => KnownNat ('S n) where
    theOne = SS theOne

type Tx :: Nat -> Type
newtype Tx n = Tx (SNat n)

type Output :: Nat -> Type
newtype Output n = Output (SNat n)

data State a = State a [a] [a]

type family NextIx (s :: State Nat) :: Nat where
    NextIx ('State n _ _) = n

type family NewOutput (s :: State Nat) :: State Nat where
    NewOutput ('State n xs ys) = 'State ('S n) (n ': xs) ys

type family NewTx (s :: State Nat) :: State Nat where
    NewTx ('State n xs ys) = 'State ('S n) xs (n ': ys)

type family Remove (xs :: [k]) (x :: k) :: [k] where
    Remove (x ': xs) x = xs
    Remove (y ': xs) x = (y ': Remove xs x)

type family UseOutput (s :: State Nat) (oid :: Nat) :: State Nat where
    UseOutput ('State n xs ys) oid = 'State n (Remove xs oid) ys

type family Member (x :: k) (xs :: [k]) :: Constraint where
    Member x (x ': xs) = ()
    Member x (_ ': xs) = Member x xs
    Member x '[]       = TypeError ('Text "unknown output or tx " ':<>: 'ShowType x)

type family CheckOutput (s :: State Nat) (oid :: Nat) :: Constraint where
    CheckOutput ('State _ xs _) oid = Member oid xs

type family CheckTx (s :: State Nat) (txid :: Nat) :: Constraint where
    CheckTx ('State _ _ xs) txid = Member txid xs

type AnimationM :: State Nat -> State Nat -> Type -> Type
data AnimationM old new a where

    Pure    :: a -> AnimationM s s a

    OutputM :: ( CheckTx old txid
               , KnownNat (NextIx old)
               )
            => Double
            -> String
            -> String
            -> Maybe String
            -> Tx txid
            -> Pos
            -> (Output (NextIx old) -> AnimationM (NewOutput old) new a)
            -> AnimationM old new a

    TxM     :: KnownNat (NextIx old)
            => Bool
            -> Pos
            -> (Tx (NextIx old) -> AnimationM (NewTx old) new a)
            -> AnimationM old new a

    InputM  :: ( CheckOutput old oid
               , CheckTx old txid
               , LT oid txid
               )
            => Maybe String
            -> Output oid
            -> Tx txid
            -> AnimationM (UseOutput old oid) new a
            -> AnimationM old new a

    DelayM  :: AnimationM old new a -> AnimationM old new a

deriving instance Functor (AnimationM old new)

return :: a -> AnimationM s s a
return = Pure

(>>=) :: AnimationM old medium a -> (a -> AnimationM medium new b) -> AnimationM old new b
Pure a                                      >>= f = f a
OutputM d address amount mdatum tx pos cont >>= f = OutputM d address amount mdatum tx pos $ (>>= f) . cont
TxM label pos cont                          >>= f = TxM label pos $ (>>= f) . cont
InputM mredeemer output tx cont             >>= f = InputM mredeemer output tx $ cont >>= f
DelayM cont                                 >>= f = DelayM $ cont >>= f

(>>) :: AnimationM old medium a -> AnimationM medium new b -> AnimationM old new b
x >> y = x >>= const y

outputM' :: ( CheckTx s txid
            , KnownNat (NextIx s)
            )
         => Double
         -> String
         -> String
         -> Maybe String
         -> Tx txid
         -> Pos
         -> AnimationM s (NewOutput s) (Output (NextIx s))
outputM' d address amount mdatum tx pos = OutputM d address amount mdatum tx pos Pure

outputM :: ( CheckTx s txid
           , KnownNat (NextIx s)
           )
        => String
        -> String
        -> Maybe String
        -> Tx txid
        -> Pos
        -> AnimationM s (NewOutput s) (Output (NextIx s))
outputM = outputM' 1.35

txM :: KnownNat (NextIx s) => Bool -> Pos -> AnimationM s (NewTx s) (Tx (NextIx s))
txM label pos = TxM label pos Pure

inputM :: ( CheckOutput s oid
          , CheckTx s txid
          , LT oid txid
          )
        => Maybe String
        -> Output oid
        -> Tx txid
        -> AnimationM s (UseOutput s oid) ()
inputM mredeemer output tx = InputM mredeemer output tx $ Pure ()

delayM :: AnimationM s s ()
delayM = DelayM $ Pure ()

runAnimationM' :: AnimationM old new a -> MS.AnimationM a
runAnimationM' (Pure a) = P.return a
runAnimationM' (OutputM d address amount mdatum (Tx n) pos cont) = do
    let txid = toInt n
    void $ MS.outputM' d address amount mdatum (MS.Tx txid) pos
    runAnimationM' $ cont $ Output theOne
runAnimationM' (TxM label pos cont) = do
    void $ MS.txM label pos
    runAnimationM' $ cont $ Tx theOne
runAnimationM' (InputM mredeemer (Output m) (Tx n) cont) = do
    let oid  = toInt m
        txid = toInt n
    MS.inputM mredeemer (MS.Output oid) (MS.Tx txid)
    runAnimationM' cont
runAnimationM' (DelayM cont) = do
    MS.delayM
    runAnimationM' cont

runAnimationM :: AnimationM ('State ('S 'Z) '[] '[]) s () -> Animation
runAnimationM m = case MS.runAnimationM $ runAnimationM' m of
    Left _   -> error "impossible case"
    Right a  -> a

reanimateM :: AnimationM ('State ('S 'Z) '[] '[]) s () -> IO ()
reanimateM = reanimate . runAnimationM
