{-# LANGUAGE OverloadedStrings #-}
module Main where

import CsvImporter
import Test.Hspec
import Data.Csv
import Data.Time
import qualified Data.Vector as V

{- UTILITY FUNCTIONS -}
justTheData :: Csv -> V.Vector Sample
justTheData c = case importGcdc c of
                    (RawAccel _ v) -> v

justTheConfig :: Csv -> ConfigInfo
justTheConfig c = case importGcdc c of
                      (RawAccel c _) -> c

decodeUnsafe = foldr const undefined . decode NoHeader

makeDateTime :: Integer -> Int -> Int -> Int -> Int -> Int -> Int -> UTCTime
makeDateTime year month day hour minute second milli =
    UTCTime (fromGregorian year month day)
            (picosecondsToDiffTime picos)
  where
    picos = totalMillis * millisToPicos
    millisToPicos = (floor 1e9) :: Integer
    totalMillis = toInteger milli
                + toInteger second * 1000
                + toInteger minute * 60000
                + toInteger hour   * 3600000 :: Integer

{- TEST DATA -}
oneLine :: Csv
oneLine = decodeUnsafe "0.0, 1.0, 2.0, 3.0"

config :: Csv
config = decodeUnsafe . mconcat $
    [ ";Title, http://www.gcdataconcepts.com, X16-mini, Analog Dev ADXL345\n"
    , ";Version, 1113, Build date, Jan  6 2016,  SN:CCDC1016B342233\n"
    , ";Start_time, 2017-06-15, 09:00:06.461\n"
    , ";Temperature, -999.00, deg C,  Vbat, 4138, mv\n"
    , ";SampleRate, 25,Hz\n"
    , ";Deadband, 0, counts\n"
    , ";DeadbandTimeout, 5,sec\n"
    , ";Time, Ax, Ay, Az\n" ]

configStartTime :: UTCTime
configStartTime = makeDateTime 2017 6 15 9 0 6 461

configSampleRate :: Float
configSampleRate = 25

lineAndConfig :: Csv
lineAndConfig = config V.++ oneLine

fiveLines :: Csv
fiveLines = decodeUnsafe . mconcat $
    [ "0.00, 1.0, 2.0, 3.0\n"
    , "0.04, 4.0, 5.0, 6.0\n"
    , "0.08, 7.0, 8.0, 9.0\n"
    , "0.12, 10.0, 11.0, 12.0\n"
    , "0.16, 13.0, 14.0, 15.0\n" ]

main :: IO ()
main = hspec $ do
    describe "CSV Data Import" $ do
      it "One line of data correctly" $ do
          let vd = justTheData oneLine
          V.length vd `shouldBe` 1
          vd V.! 0 `shouldBe` Sample 1.0 2.0 3.0
      it "Five lines of data correctly" $ do
          let vd = justTheData fiveLines
          V.length vd `shouldBe` 5
          vd V.! 0 `shouldBe` Sample 1 2 3
          vd V.! 1 `shouldBe` Sample 4 5 6
          vd V.! 2 `shouldBe` Sample 7 8 9
          vd V.! 3 `shouldBe` Sample 10 11 12
          vd V.! 4 `shouldBe` Sample 13 14 15
      let c = justTheConfig config
      it "Configuration Sample rate" $ do
          sampleRate c `shouldBe` configSampleRate
      it "Configuration Date" $ do
          startTime c `shouldBe` configStartTime
      it "Some header info and a line of real data" $ do
          let (RawAccel c v) = importGcdc lineAndConfig
          startTime c `shouldBe` configStartTime
          sampleRate c `shouldBe` configSampleRate
          V.length v `shouldBe` 1
          v V.! 0 `shouldBe` Sample 1.0 2.0 3.0
      it "Incorrectly-formatted data" $ do
          pending
