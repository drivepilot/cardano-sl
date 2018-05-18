{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
    -- * Events
  , Event(..)
    -- * Interpreter
  , IntEventT
  , runIntEvent
  , runIntEvent_
  , intEvents
  , intEvent
    -- * Generators
    -- ** Test graph output
  , TgoParams(..)
  , defTgoParams
  , testGraphOutput
  ) where

import           Universum

import           Control.Lens ((%=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import qualified Data.Text.Buildable
import           Formatting (bprint, (%), sformat, build)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import           InputSelection.Policy
import           Util.Histogram
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event h a =
    Deposit (Utxo h a)
  | Pay [Output a]
  | NextSlot

{-------------------------------------------------------------------------------
  Interpreter monad
-------------------------------------------------------------------------------}

data IntParams h a m = IntParams {
      intParamsPolicy     :: InputSelectionPolicy h a (IntEventT h a m)

      -- | Fixed change address
      --
      -- We don't care about privacy for these evaluations, so we stick with
      -- a fixed change address.
    , intParamsChangeAddr :: a
    }

data IntState h a = IntState {
      _intStateUtxo      :: Utxo h a
    , _intStatePending   :: Utxo h a
    , _intStateStats     :: IntStats
    , _intStateFreshHash :: Int
    }

stateHistogram :: BinSize -> IntState h a -> Histogram
stateHistogram binSize =
      histogram binSize
    . map fromIntegral
    . outputs
    . _intStateUtxo
  where
    outputs :: Utxo h a -> [Value]
    outputs = map (outVal . snd) . utxoToList

data IntStats = IntStats

instance Buildable IntStats where
  build IntStats = bprint "<TODO: statistics>"

initStats :: IntStats
initStats = IntStats

initState :: Utxo h a -> IntState h a
initState utxo = IntState {
      _intStateUtxo      = utxo
    , _intStatePending   = utxoEmpty
    , _intStateStats     = initStats
    , _intStateFreshHash = 1
    }

newtype IntEventT h a m x = IntEventT {
    unIntEventT :: ReaderT (IntParams h a m) (StateT (IntState h a) m) x
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (IntState h a)
           , MonadReader (IntParams h a m)
           )

makeLenses ''IntState

instance MonadTrans (IntEventT h a) where
  lift = IntEventT . lift . lift

instance CanRunQuickCheck m => CanRunQuickCheck (IntEventT h a m) where
  liftQuickCheck = lift . liftQuickCheck

instance Monad m => CanRunPolicy (IntEventT h a m) a where
  genChangeAddr = asks intParamsChangeAddr
  genFreshHash  = intStateFreshHash <<+= 1

runIntEvent :: Monad m
            => IntParams h a m
            -> Utxo h a
            -> IntEventT h a m x
            -> m (x, IntStats)
runIntEvent params utxo act =
    second (view intStateStats) <$>
      runStateT (runReaderT (unIntEventT act) params) (initState utxo)

runIntEvent_ :: Monad m
             => IntParams h a m
             -> Utxo h a
             -> IntEventT h a m x
             -> m IntStats
runIntEvent_ params utxo = fmap snd . runIntEvent params utxo

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

intEvents :: forall m h a x. (Monad m, Hash h a)
          => [Event h a]
          -> (IntState h a -> m x) -- ^ Observe the state after each event
          -> IntEventT h a m [x]
intEvents events observe = mapM go events
  where
    go :: Event h a -> IntEventT h a m x
    go ev = do intEvent ev
               lift . observe =<< get

intEvent :: forall m h a. (Monad m, Hash h a)
         => Event h a -> IntEventT h a m ()
intEvent = go
  where
    go :: Event h a -> IntEventT h a m ()
    go (Deposit new) = intStateUtxo %= utxoUnion new
    go (Pay outs)    = pay outs
    go NextSlot      = do pending <- use intStatePending
                          intStateUtxo    %= utxoUnion pending
                          intStatePending .= utxoEmpty

pay :: (Monad m, Hash h a)
    => [Output a] -> IntEventT h a m ()
pay outs = do
    IntParams{..} <- ask
    utxo <- use intStateUtxo
    mtx  <- intParamsPolicy utxo outs
    case mtx of
      Right tx -> do
        intStateUtxo    %= utxoRemoveInputs (trIns tx)
        intStatePending %= utxoUnion (trUtxo tx)
      Left _err ->
        -- TODO: record some stats
        return ()

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

data TgoParams = TgoParams {
      tgoParamsMin   :: Value
    , tgoParamsMax   :: Value
    , tgoParamsIncr  :: Value
    , tgoParamsCount :: Int
    }

defTgoParams :: TgoParams
defTgoParams = TgoParams {
      tgoParamsMin   = 10
    , tgoParamsMax   = 100
    , tgoParamsIncr  = 10
    , tgoParamsCount = 10
    }

-- | Series of events to test the graph output
--
-- For each output value in the range
--
-- > (min, min + incr .. max)
--
-- we generate @count@ outputs. After that, we generate transactions that
-- use up those outputs in precisely the same order, generating no change.
--
-- The point is that this allows us to visually see immediately if the resulting
-- graph animation makes sense.
testGraphOutput :: TgoParams -> [Event GivenHash ()]
testGraphOutput TgoParams{..} = concat [
      [ Deposit $ utxoFromList [(Input (GivenHash (fromIntegral n)) m, Output () n)]
      | n <- vals, m <- ixs
      ]
    , [ Pay $ [Output () n]
      | n  <- vals, _m <- ixs
      ]
    ]
  where
    vals :: [Value]
    vals = [tgoParamsMin, tgoParamsMin + tgoParamsIncr .. tgoParamsMax]

    ixs :: [Word32]
    ixs = [1 .. fromIntegral tgoParamsCount]

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

evaluateInputPolicies :: IO ()
evaluateInputPolicies = do
    counter <- newIORef Map.empty

    let observeState :: FilePath -> BinSize -> IntState h a -> IO Text
        observeState prefix binSize st = do
            ix <- atomicModifyIORef counter (nextIndex prefix)
            let filename = printf "%08d" ix
                filepath = prefix </> filename
            writeHistogram filepath hist
            return $ sformat
                ( "set output '" % build % ".png'\n"
                % "plot '" % build % "' using 1:2 with boxes\n"
                )
                filename
                filename
          where
            hist = stateHistogram binSize st

    let exactMatchParams :: IntParams GivenHash () IO
        exactMatchParams = IntParams {
            intParamsPolicy     = exactSingleMatchOnly
          , intParamsChangeAddr = ()
          }

    (plotInstrs, stats) <- runIntEvent exactMatchParams utxoEmpty $
      intEvents (testGraphOutput defTgoParams) $ observeState "exact/" 10
    writeFile "exact/exact.gnuplot" $ sformat
      ( "set grid\n"
      % "set xrange [5:105]\n"
      % "set yrange [0:10.5]\n"
      % "set term png\n"
      % build
      )
      (mconcat plotInstrs)
    putStrLn $ pretty stats


  where
    nextIndex :: FilePath -> Map FilePath Int -> (Map FilePath Int, Int)
    nextIndex prefix counts =
        case Map.lookup prefix counts of
          Nothing -> (Map.insert prefix      2  counts, 1)
          Just c  -> (Map.insert prefix (c + 1) counts, c)



{-

input selection
coin selection

bitcoinj coin selection? ("multiple classes" -- multiple policies)

https://github.com/bitcoin/bitcoin/issues/7664

See ApproximateBestSubset in wallet.cpp.

sweep?



-}


{-
http://murch.one/wp-content/uploads/2016/11/erhardt2016coinselection.pdf
"An Evaluation of Coin Selection Strategies", Master’s Thesis, Mark Erhardt

2.3.4
A transaction output is labeled as dust when its value is similar to the cost of
spending it.Precisely, Bitcoin Core sets the dust limit to a value where spending an
2.3. Transactions 7
output would exceed 1/3 of its value. T

https://youtu.be/vnHQwYxB08Y?t=39m


https://www.youtube.com/watch?v=IKSSWUBqMCM

companies; hundreds of entries in UTxO
individuals: tens of entries

batch payments!
  (Harding, BitcoinTechTalk -- safe up to 80% of transaction fees)

coin selection --
  relatively minor importance
    batching, better representation (SegWit), .. much larger impact
    coin selection only a few percent savings

* FIFO is actually a reasonable strategy (!)
* So is random
    self correcting -- if you have a large amount of small inputs,
    they'll be more likely to be picked!
    (i.e, if 90% of the wallet is small inputs, 90% change of picking them!)

Branch&Bound seems to do exhaustive search (backtracking algorithm) to find
exact matches, coupled with random selection.

A Traceability Analysis of Monero’s Blockchain
https://eprint.iacr.org/2017/338.pdf
-}
