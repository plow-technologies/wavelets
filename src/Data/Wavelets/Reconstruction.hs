{-# LANGUAGE OverloadedStrings #-}


module Data.Wavelets.Reconstruction where 
import Prelude hiding (map,maximum,minimumx)
import Data.Wavelets 
import Numeric.Statistics
import Statistics.Sample
-- import Data.Vector.Storable -- hiding (map)
-- import Linear

-- |explicitly name a Fractional and Ord instance on the idwt function
fidwt ::  Int -> WaveletFilter Double -> WaveletPacker Double c -> c -> [Double]
fidwt = idwt


-- | reconstruct the time series raw, without worrying about scaling
reconstructTimeSeries :: Int -> WaveletFilter Double -> WaveletPacker Double c -> c -> [Double]
reconstructTimeSeries i wft wptc c = init $ fidwt i wft wptc c

reconstructHaarTimeSeries :: Int -> [[Double]] -> [Double]
reconstructHaarTimeSeries i c = reconstructTimeSeries i haar wp_separate c


