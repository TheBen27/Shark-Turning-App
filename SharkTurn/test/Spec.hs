module Main where

import CsvImporter
import Test.Hspec
import Data.Csv
import qualified Data.Vector as V

justTheData :: Csv -> V.Vector Sample
justTheData c = case importGcdc c of
                    (RawAccel _ v) -> v

main :: IO ()
main = hspec $ do
    describe "CSV Data Import" $ do
      it "One line of data correctly" $ do
          pending
      it "Some header info and a line of real data" $ do
          pending
      it "Five lines of data correctly" $ do
          pending
      it "Incorrectly-formatted file" $ do
          pending
