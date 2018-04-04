-----------------------------------------------------------------------------
--
-- Module      :  WindowerSpec
-- Copyright   :  2018 Author name here
-- License     :  BSD3
--
-- Maintainer  :  example@example.com
-- Stability   :
-- Portability :
--
-- | Spec tests for the windower.
--
-----------------------------------------------------------------------------

module WindowerSpec (
spec
) where

import Windower
import Models
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Array.Repa as R

{-
 - Utility functions
 -}

-- |Check if two WindowedAccels have the same data
windowSameData :: WindowedAccel -> WindowedAccel -> Bool
windowSameData (WindowedAccel _ ax1 ay1 az1) (WindowedAccel _ ax2 ay2 az2) =
    (ax1 `weq` ax2) && (ay1 `weq` ay2) && (az1 `weq` az2)
  where
    weq (WindowedAxis a1) (WindowedAxis a2) = a1 == a2

{-
 - We should probably start with the trivial case where overlap == 0.
-}

fakeConfig :: ConfigInfo
fakeConfig = undefined

fakeRaw :: RawAccel
fakeRaw = makeFakeRaw [1..20]

makeFakeRaw :: [Float] -> RawAccel
makeFakeRaw xs = RawAccel fakeConfig $
    V.fromList . fmap (\x -> Sample x x x) $ xs

makeFakeWindowed :: Int -> Int -> [Float] -> WindowedAccel
makeFakeWindowed r c xs = 
    let fakeAxis :: WindowedAxis
        fakeAxis = WindowedAxis $ R.fromListUnboxed (R.Z R.:. r R.:. c) xs
    in  WindowedAccel fakeConfig fakeAxis fakeAxis fakeAxis

--[1,2,3,4,5]
--[6,7,8,9,10]
--[11,12,13,14,15],
--[16,17,18,19,20]
fakeWindowedSizeFiveNoOverlap :: WindowedAccel
fakeWindowedSizeFiveNoOverlap =
    makeFakeWindowed 4 5 [1..20]

--[1..20], size 5, overlap 5
--Window size: 5 + 10 = 15
--Number of windows: (20 - 5) / (5 + 5) = 15 / 10 = 1
--Window 1: sind = 1, eind = 15
--[1..15]
fakeWindowedSizeFiveOverlapFive :: WindowedAccel
fakeWindowedSizeFiveOverlapFive =
  makeFakeWindowed 1 15 [1..15]

spec :: Spec
spec = describe "Windowing tests" $ do
         it "Window with no overlap" $ do
           let wd = windowify 5 0 fakeRaw
           windowSameData wd fakeWindowedSizeFiveNoOverlap `shouldBe` True
         it "Five size, five overlap [1..20]" $ do
           let wd = windowify 5 5 fakeRaw
           windowSameData wd fakeWindowedSizeFiveOverlapFive `shouldBe` True
