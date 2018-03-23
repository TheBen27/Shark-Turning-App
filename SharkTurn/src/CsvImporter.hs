{-# LANGUAGE OverloadedStrings #-}
module CsvImporter where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Data.Time
import Data.Time.Format
import Data.Validation

import Data.Csv

data Sample = Sample !Float !Float !Float deriving (Show, Eq)

data RawAccel = RawAccel ConfigInfo (V.Vector Sample)

newtype Window = Window (V.Vector Sample)

data WindowedAccel = WindowedAccel ConfigInfo (V.Vector Window)

data ConfigInfo = ConfigInfo { startTime  :: UTCTime
                             , sampleRate :: Float }

data ParseError = NoSampleRate
                | NoStartDate
                | NoLoadedData
                | CouldNotParse

data CsvLine = ConfigLine B.ByteString (V.Vector B.ByteString)
             | DataLine   Float Float Float Float deriving (Show)

isData :: CsvLine -> Bool
isData (DataLine _ _ _ _) = True
isData (ConfigLine _ _) = False

isConfig = not . isData

instance FromRecord CsvLine where
    parseRecord r
        | (B.head . V.head) r == ';' =
              let k = (B.tail . V.head) r
                  v = V.tail r
              in  ConfigLine <$> (parseField k) <*> (traverse parseField v)
        | otherwise = DataLine <$>
                      r `index` 0 <*>
                      r `index` 1 <*>
                      r `index` 2 <*>
                      r `index` 3

-- |Import a GCDC accelerometer file.
importGcdc :: BL.ByteString -> Validation [ParseError] RawAccel
importGcdc b =
    case parseCsv b of
        (Failure f) -> Failure f
        (Success v) -> RawAccel <$> importGcdcConfig v <*> importGcdcData v

{-
 - parseCsv b -> Validation . .
 -
 - Take this value and push it into importGcdcData and importGcdcConfig
 -}

importGcdcData :: V.Vector (CsvLine) -> Validation [ParseError] (V.Vector Sample)
importGcdcData = validate [NoLoadedData] V.null . load
  where
    load = fmap toSample . V.filter isData
    toSample (DataLine t x y z) = Sample x y z


importGcdcConfig :: V.Vector (CsvLine) -> Validation [ParseError] ConfigInfo
importGcdcConfig = do
    st <- startTime
    sr <- sampleRate
    return $ ConfigInfo <$> st <*> sr
  where
    startTime = undefined
    sampleRate = undefined

parseCsv :: BL.ByteString -> Validation [ParseError] (V.Vector (CsvLine))
parseCsv = liftError (const [CouldNotParse]) . decode NoHeader
