{-# LANGUAGE OverloadedStrings #-}
module Main where

import CsvImporter
import Windower
import Models
import Data.Csv (Header)
import Data.Validation
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

data FeatureTable = FeatureTable
data Labels = Labels

makeFeatures :: WindowedAccel -> FeatureTable
makeFeatures = undefined

predict :: FeatureTable -> Labels
predict = undefined

writeLabels :: Labels -> B.ByteString
writeLabels = undefined

main :: IO ()
main = do
    [inFname, outFname] <- getArgs
    inFile <- BL.readFile inFname
    let output = writeLabels
               . predict
               . makeFeatures
               . windowify 25 12
               <$> importGcdc inFile
    case output of
        (Failure errs) ->
          putStrLn $ "Couldn't load data:" ++ (unlines . map show) errs
        (Success out) -> B.writeFile outFname out

{-
 - Import the data
 - Window it
 - Generate feature table
 - Run classifiers to get predictions
 - Return predictions and confidences
 -}
