module Util.Histogram (
    -- * Basic definitions
    Bin
  , Count
  , Histogram
    -- * Output
  , writeHistogram
    -- * Construction
  , BinSize
  , histogram
  ) where

import           Universum

import qualified Data.Map as Map
import           System.IO

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

type Bin       = Int
type Count     = Int
type Histogram = [(Bin, Count)]

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | Write out histogram
--
-- Example plot using gnuplot:
--
-- > set grid
-- > set xrange [5:105]  -- for bins 10, 20, .. 100
-- > set yrange [0:10.5] -- for counts from 0 .. 10
-- > plot 'data.csv' using 1:2 with boxes
writeHistogram :: FilePath -> Histogram -> IO ()
writeHistogram fp hist =
    withFile fp WriteMode $ \h ->
      forM_ hist $ \(step, count) ->
        hPutStrLn h $ show step ++ "\t" ++ show count

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

type BinSize = Int

-- | Construct histogram by counting all the doubles per bin
histogram :: BinSize -> [Double] -> Histogram
histogram binSize = Map.toList . go Map.empty
  where
    go :: Map Bin Count -> [Double] -> Map Bin Count
    go acc []     = acc
    go acc (d:ds) = let bin = floor (d / fromIntegral binSize) * binSize
                    in go (Map.alter incr bin acc) ds

    incr :: Maybe Count -> Maybe Count
    incr Nothing  = Just 1
    incr (Just n) = Just (n + 1)
