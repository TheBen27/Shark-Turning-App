{-# LANGUAGE OverloadedStrings #-}
module CsvImporter
    ( Sample (..)
    , Window (..)
    , ConfigInfo (..)
    , WindowedAccel (..)
    , RawAccel (..)
    , importGcdc
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format

import Data.Csv

data Sample = Sample !Float !Float !Float deriving (Show, Eq)

newtype Window = Window (V.Vector Sample)

data ConfigInfo = ConfigInfo { startTime  :: UTCTime
                             , sampleRate :: Float }

data WindowedAccel = WindowedAccel ConfigInfo (V.Vector Window)

data RawAccel = RawAccel ConfigInfo (V.Vector Sample)

-- |Import a GCDC accelerometer file.
importGcdc :: Csv -> RawAccel
importGcdc = RawAccel <$> importGcdcConfig
                      <*> importGcdcData


importGcdcData :: Csv -> V.Vector Sample
importGcdcData = V.mapMaybe byRecord
  where
    byRecord :: Record -> Maybe Sample
    byRecord v = if v V.! 0 /= ""
                 then Sample <$> rfloat (v V.! 1)
                             <*> rfloat (v V.! 2)
                             <*> rfloat (v V.! 3)
                 else Nothing
    
    rfloat :: BS.ByteString -> Maybe Float
    rfloat s = case (reads . BS.unpack) s of
                   [] -> Nothing
                   ((f,_):_) -> Just f 


importGcdcConfig :: Csv -> ConfigInfo
importGcdcConfig csv = ConfigInfo (fromJust startTime) sampleRate
  where
    startTime :: Maybe UTCTime
    startTime = let v = getKey "Start_time"
                    s = mconcat [v V.! 0, " ", v V.! 1]
                in  parseTimeM True defaultTimeLocale
                               "%F %T" (BS.unpack s)

    sampleRate = (read . BS.unpack . V.head . getKey) "SampleRate"

    -- Separate config section into key-values pairs
    getKey k = snd . fromJust . V.find ((== k) . fst) $ keys
    keys :: V.Vector (BS.ByteString, V.Vector BS.ByteString)
    keys = fmap (\r -> (V.head r, V.tail r))
         . V.filter (BS.null . V.head) $ csv
