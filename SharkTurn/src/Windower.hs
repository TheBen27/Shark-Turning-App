module Windower where

import Models
import qualified Data.Array.Repa as R
import Data.Array.Repa.Shape
import qualified Data.Vector as V

{-
 - We're effectively porting the MATLAB code here
 -
 - total_window_size = window_size * 2 + window_overlap;
 - sind = (win - 1) * (window_overlap + window_size) + 1;
 - eind = sind + total_window_size - 1;
 - num_windows = floor((length(accel_r) - window_overlap) / (window_overlap + window_size))
 -
 - Given the current window index and position in the window, find the original raw index.
 - Example: size of 5, overlap of 0.
 - f(wi, wo) = wi * 5 + wo
 -
 - Example 2: size of 5, overlap of 5.
 - f(0, 0) = 0
 - f(0, 14) = 14
 - f(1, 0) = 10
 -
 - I think, generally, that
 - f(wi, wo) = wi * (size + overlap) + wo
 -}

windowify :: Int -> Int -> RawAccel -> WindowedAccel
windowify size overlap (RawAccel c d) = WindowedAccel c ax ay az
  where
    rx = R.fromListUnboxed rsh (V.toList . fmap (\(Sample x _ _) -> x) $ d)
    ry = R.fromListUnboxed rsh (V.toList . fmap (\(Sample _ y _) -> y) $ d)
    rz = R.fromListUnboxed rsh (V.toList . fmap (\(Sample _ _ z) -> z) $ d)
    rsh = R.Z R.:. V.length d
    [ax, ay, az] = (WindowedAxis . R.computeS . windowAxis) <$> [rx, ry, rz]
    windowAxis r =
        R.traverse r
            (const $ R.Z R.:. numWindows R.:. totalWindowSize)
            (\f sh -> let [wi,wo] = listOfShape sh
                          i = wi * (size + overlap) + wo
                          si = R.Z R.:. i
                      in  f si)

    totalWindowSize = size * 2 + overlap
    numWindows = V.length d - overlap `div` overlap + size
