{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

module AnimationM
    ( MonadError (..)
    , Pos
    , Tx
    , Output
    , AnimationM
    , txM
    , outputM
    , inputM
    , delayM
    , runAnimationM
    , reanimateM
    ) where

import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.RWS.Strict
import           Data.Foldable            (toList)
import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IM
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import           Data.List                (foldl')
import           Data.Maybe               (fromMaybe)
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.Text                (pack)
import           Optics
import           Optics.State.Operators
import           Reanimate
import           System.Exit              (ExitCode (..), exitWith)
import           System.IO                (hPutStrLn, stderr)

import qualified Core                     as C

type Pos = (Double, Double)
type PosMap = IntMap Pos

data S = S
    { _sNextIx  :: !Int
    , _sNextTx  :: !Int
    , _sOutputs :: !PosMap
    , _sTxs     :: !PosMap
    , _sUTxOs   :: !IntSet
    } deriving (Show, Read, Eq, Ord)

newtype Tx = Tx {_getTx :: Int}
    deriving (Show, Read, Eq, Ord)

newtype Output = Output {_getOutput :: Int}
    deriving (Show, Read, Eq, Ord)

makeLenses ''S
makeLenses ''Tx
makeLenses ''Output

data Command =
      OutputCmd String String String Int Int
    | InputCmd String Int Int
    | TxCmd (Maybe String) Int
    | Delay
    deriving (Show, Read, Eq, Ord)

newtype AnimationM a = AnimationM (RWST () (Seq Command) S (Either String) a)
    deriving (Functor, Applicative, Monad, MonadError String)

runAnimationM' :: AnimationM () -> Either String ([Command], PosMap, PosMap)
runAnimationM' (AnimationM m) = case runRWST m () s of
    Left err           -> Left err
    Right ((), s', xs) -> Right (toList xs, s' ^. sOutputs, s' ^. sTxs)
  where
    s = S
        { _sNextIx  = 1
        , _sNextTx  = 1
        , _sOutputs = IM.empty
        , _sTxs     = IM.empty
        , _sUTxOs   = IS.empty
        }

nextIx :: MonadState S m => m Int
nextIx = do
    i <- use sNextIx
    sNextIx %= succ
    return i

checkOutput :: (MonadState S m, MonadError String m) => Output -> m ()
checkOutput o@(Output oid) = do
    m <- use $ sOutputs % at oid
    case m of
        Nothing -> throwError $ "undefined output " ++ show o
        Just _  -> do
            s <- use sUTxOs
            if oid `IS.member` s
                then return ()
                else throwError $ "output " ++ show o ++ " already consumed"

checkTx :: (MonadState S m, MonadError String m) => Tx -> m ()
checkTx tx@(Tx txid) = do
    m <- use $ sTxs % at txid
    case m of
        Nothing -> throwError $ "undefined transaction " ++ show tx
        Just _  -> return ()

txM :: Bool -> Pos -> AnimationM Tx
txM named pos = AnimationM $ do
    txId <- nextIx
    ml   <- if named then do
                l <- use sNextTx
                sNextTx %= succ
                return $ Just $ show l
            else
                return Nothing
    sTxs % at txId .= Just pos
    tell $ Seq.singleton $ TxCmd ml txId
    return $ Tx txId

outputM :: String -> Maybe String -> String -> Tx -> Pos -> AnimationM Output
outputM address mdatum value tx@(Tx txId) pos = AnimationM $ do
    checkTx tx
    oid <- nextIx
    sOutputs % at oid .= Just pos
    sUTxOs            %= IS.insert oid
    tell $ Seq.singleton $ OutputCmd address (fromMaybe "" mdatum) value txId oid
    return $ Output oid

inputM :: Maybe String -> Output -> Tx -> AnimationM ()
inputM mredeemer o@(Output oid) tx@(Tx txid)
    | oid >= txid = throwError $ "transaction " ++ show tx ++ " is too old to consume output " ++ show o
    | otherwise   = AnimationM $ do
        checkOutput o
        checkTx tx
        tell $ Seq.singleton $ InputCmd (fromMaybe "" mredeemer) oid txid

delayM :: AnimationM ()
delayM = AnimationM $ tell $ Seq.singleton Delay

insOuts :: [Command] -> IntMap (Int, Int)
insOuts = foldl' f IM.empty
  where
    f :: IntMap (Int, Int) -> Command -> IntMap (Int, Int)
    f m (OutputCmd _ _ _ txid _) = m & ix txid % _2 %~ succ
    f m (InputCmd _ _ txid)      = m & ix txid % _1 %~ succ
    f m (TxCmd _ txid)           = m & at txid      ?~ (0, 0)
    f m Delay                    = m

offset :: Int -> Int -> Double
offset total i =
  let
    d = 2 / (2 * fromIntegral total)
  in
    1 - (2 * fromIntegral i + 1) * d

type M a = RWS (PosMap, PosMap, IntMap (Int, Int)) () (IntMap (Int, Int)) a

getOffset :: Lens (Int, Int) (Int, Int) Int Int -> Int -> M Double
getOffset l txid = do
    x <- IM.findWithDefault (0, 0) txid <$> get
    t <- view l . (IM.! txid) <$> asks (view _3)
    at txid .= Just (x & l %~ succ)
    return $ offset t $ x ^. l

getOutputPos :: Int -> M Pos
getOutputPos oid = (IM.! oid)  <$> asks (view _1)

getTxPos :: Int -> M Pos
getTxPos txid = (IM.! txid)  <$> asks (view _2)

stepM :: Command -> M Animation
stepM (OutputCmd address datum value txid oid) = do
    oPos   <- getOutputPos oid
    (x, y) <- getTxPos txid
    dy     <- getOffset _2 txid
    return $ C.output
        (pack address)
        (pack datum)
        (pack value)
        (x, y + dy)
        oPos
stepM (InputCmd redeemer oid txid) = do
    oPos   <- getOutputPos oid
    (x, y) <- getTxPos txid
    dy     <- getOffset _1 txid
    return $ C.input
        (pack redeemer)
        (x, y + dy)
        oPos
stepM (TxCmd mlabel txid) = do
    txPos <- getTxPos txid
    return $ C.txWithLabel mlabel txPos
stepM Delay = return $ pause 1

stepsM :: [Command] -> M Animation
stepsM []           = return $ pause 0
stepsM (cmd : cmds) = do
    x <- stepM cmd
    y <- stepsM cmds
    return $ x `andThen` y

runAnimationM :: AnimationM () -> Either String Animation
runAnimationM m = case runAnimationM' m of
    Left err                -> Left err
    Right (cmds, outs, txs) -> Right $ fst $ evalRWS (stepsM cmds) (outs, txs, insOuts cmds) IM.empty

reanimateM :: AnimationM () -> IO ()
reanimateM m = do
    case runAnimationM m of
        Left err -> hPutStrLn stderr ("ERROR: " ++ err) >> exitWith (ExitFailure 1)
        Right x  -> reanimate x

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

