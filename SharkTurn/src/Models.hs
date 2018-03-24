module Models where

import qualified Data.Vector as V
import Data.Time

data Sample = Sample !Float !Float !Float deriving (Show, Eq)

newtype Window = Window (V.Vector Sample) deriving (Show)

data WindowedAccel = WindowedAccel ConfigInfo (V.Vector Window) deriving (Show)

data RawAccel = RawAccel ConfigInfo (V.Vector Sample) deriving (Show)

data ConfigInfo = ConfigInfo { startTime  :: LocalTime
                             , sampleRate :: Float } deriving (Show)
