module Models where

import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import Data.Time

data Sample = Sample !Float !Float !Float deriving (Show, Eq)

newtype Window = Window (V.Vector Sample) deriving (Show)

newtype WindowedAxis = WindowedAxis (R.Array R.U R.DIM2 Float) deriving (Show)
data WindowedAccel = WindowedAccel ConfigInfo WindowedAxis WindowedAxis WindowedAxis deriving (Show)

data RawAccel = RawAccel ConfigInfo (V.Vector Sample) deriving (Show)

data ConfigInfo = ConfigInfo { startTime  :: LocalTime
                             , sampleRate :: Float } deriving (Show)
