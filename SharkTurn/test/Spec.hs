{-# LANGUAGE OverloadedStrings #-}
module Main where

import CsvImporter
import Test.Hspec
import Data.Csv
import Data.Time
import Data.Validation
import Data.Fixed
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BL

makeDateTime :: Integer -> Int -> Int -> Int -> Int -> Int -> Int -> LocalTime
makeDateTime year month day hour minute second milli =
    LocalTime (fromGregorian year month day)
              (TimeOfDay hour minute (MkFixed . toInteger $ picos))
  where
    picos = milli * floor 1e9 + second * floor 1e12

oneLine = "0.0, 1.0, 2.0, 3.0" :: BL.ByteString

config :: BL.ByteString
config = mconcat $
    [ ";Title, http://www.gcdataconcepts.com, X16-mini, Analog Dev ADXL345\n"
    , ";Version, 1113, Build date, Jan  6 2016,  SN:CCDC1016B342233\n"
    , ";Start_time, 2017-06-15, 09:00:06.461\n"
    , ";Temperature, -999.00, deg C,  Vbat, 4138, mv\n"
    , ";SampleRate, 25,Hz\n"
    , ";Deadband, 0, counts\n"
    , ";DeadbandTimeout, 5,sec\n"
    , ";Time, Ax, Ay, Az\n" ]

lineAndConfig :: BL.ByteString
lineAndConfig = mconcat [config, "\n", oneLine]

fiveLines :: BL.ByteString
fiveLines = mconcat $
    [ "0.00, 1.0, 2.0, 3.0\n"
    , "0.04, 4.0, 5.0, 6.0\n"
    , "0.08, 7.0, 8.0, 9.0\n"
    , "0.12, 10.0, 11.0, 12.0\n"
    , "0.16, 13.0, 14.0, 15.0\n" ]

configStartTime :: LocalTime
configStartTime = makeDateTime 2017 6 15 9 0 6 461

configSampleRate :: Float
configSampleRate = 25

main :: IO ()
main = hspec $ do
    describe "CSV Data Import" $ do
      it "One line of data" $ do
         let p = parseCsv oneLine
             o = bindValidation p importGcdcData
         case o of
             (Failure f) -> expectationFailure . show $ f
             (Success v) -> do
                 V.length v `shouldBe` 1
                 v V.! 0 `shouldBe` Sample 1 2 3
      it "Five lines of data correctly" $ do
         let p = parseCsv fiveLines
             o = bindValidation p importGcdcData
         case o of
             (Failure f) -> expectationFailure . show $ f
             (Success v) -> do
                 V.length v `shouldBe` 5
                 v V.! 0 `shouldBe` Sample 1 2 3
                 v V.! 1 `shouldBe` Sample 4 5 6
                 v V.! 2 `shouldBe` Sample 7 8 9
                 v V.! 3 `shouldBe` Sample 10 11 12
                 v V.! 4 `shouldBe` Sample 13 14 15
      it "Configuration Sample rate" $ do
         pending
      it "Configuration Date" $ do
         pending
      it "Some header info and a line of real data" $ do
         pending
      it "Incorrectly-formatted data" $ do
         pending
