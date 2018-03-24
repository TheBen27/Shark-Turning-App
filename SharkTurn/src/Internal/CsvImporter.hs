{-# LANGUAGE OverloadedStrings #-}
module Internal.CsvImporter where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Data.Time
import Data.Time.Format
import Data.Validation
import Data.Csv

import Models

data ParseError = NoSampleRate
                | NoStartTime
                | NoLoadedData
                | CouldNotParse String deriving (Eq, Show)

data CsvLine = DataLine Float Float Float Float 
             | SampleRate Float
             | StartTime  LocalTime
             | OtherLine deriving (Show)

isSampleRate :: CsvLine -> Bool
isSampleRate (SampleRate _) = True
isSampleRate _ = False

isStartTime :: CsvLine -> Bool
isStartTime (StartTime _) = True
isStartTime _ = False

instance FromRecord CsvLine where
    parseRecord r
        | ';' `B.elem` V.head r =
              let k = (B.tail . V.head) r
                  v = V.tail r
              in  case k of
                      "SampleRate" -> SampleRate <$> r `index` 1
                      "Start_time" -> do
                          d <- r `index` 1
                          t <- r `index` 2
                          let str = mconcat [d, " ", t]
                          case parseTimeM True defaultTimeLocale "%F %_T%Q" str of
                              (Nothing) -> fail $ "Couldn't decode " ++ str
                              (Just t) -> return $ StartTime t
                      _ -> return OtherLine
        | otherwise = DataLine <$>
                      r `index` 0 <*>
                      r `index` 1 <*>
                      r `index` 2 <*>
                      r `index` 3

isData :: CsvLine -> Bool
isData (DataLine _ _ _ _) = True
isData _ = False

-- |Import a GCDC accelerometer file.
importGcdc :: BL.ByteString -> Validation [ParseError] RawAccel
importGcdc b =
    case parseCsv b of
        (Failure f) -> Failure f
        (Success v) -> RawAccel <$> importGcdcConfig v <*> importGcdcData v

importGcdcData :: V.Vector (CsvLine) -> Validation [ParseError] (V.Vector Sample)
importGcdcData = validate [NoLoadedData] (not . V.null) . load
  where
    load = fmap toSample . V.filter isData
    toSample (DataLine t x y z) = Sample x y z

importGcdcConfig :: V.Vector (CsvLine) -> Validation [ParseError] ConfigInfo
importGcdcConfig ls = ConfigInfo <$> startTime ls <*> sampleRate ls
  where
    startTime v = case V.find isStartTime v of
                      (Just (StartTime t)) -> Success t
                      Nothing -> Failure [NoStartTime]
    sampleRate v = case V.find isSampleRate v of
                      (Just (SampleRate s)) -> Success s
                      Nothing -> Failure [NoSampleRate]

parseCsv :: BL.ByteString -> Validation [ParseError] (V.Vector (CsvLine))
parseCsv = liftError (\s -> [CouldNotParse s]) . decode NoHeader
