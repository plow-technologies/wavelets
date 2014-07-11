module Data.Wavelets.ScalingSpec (main, spec) where

import Test.Hspec
import Data.Wavelets.Reconstruction
import Data.Wavelets.Scaling
import Data.Wavelets
-- import System.IO
-- import Numeric.Statistics
-- import qualified Data.Vector.Storable as V

{-| The test waveletData below was transformed into several of the result dataFiles |-}
-- 3 sinusoids added together to make an interesting data set that is easy to understand 
testWaveletData :: [Double]
testWaveletData = [ ((sin (pi*t*2))+ (sin (pi * t * 2.1) + (sin (pi * t * 2.002))))* 12 | t <- [1 .. 1000] ]

impulse = replicate 499 1
          ++ replicate 1 200
          ++ replicate 500 1
impulseAvg = (1.3956047904191924)



xs :: [Int]
xs = [0,15,30,60,75,90,105,143,162,201,206,209,250,260,270,280,295,301,310,328,335,354,378,394,401,419,434,449,479,494,509,524,562,581,620,625,628,669,679,689,699,714,720,729,747,754,773,797,813,820,820,835,850,880,895,910,925,963,982,1021,1026,1029,1070,1080,1090,1100,1115,1121,1130,1148,1155,1174,1198,1214,1221,1239,1254,1269,1299,1314,1329,1344,1382,1401,1440,1440,1455,1470,1500,1515,1530,1545,1583,1602,1641,1646,1649,1690,1700,1710,1720,1735,1741,1750,1768,1775,1794,1818,1834,1841,1859,1874,1889,1919,1934,1949,1964,2002,2021,2060,2065,2068,2109,2119,2129,2139,2154,2160]

ys :: [Double]
ys = [1.0,6.0,4.0,2.0,1.0,5.0,5.0,2.0,3.0,5.0,6.0,4.0,5.0,6.0,1.0,1.0,5.0,2.0,4.0,5.0,6.0,7.0,41.0,1.0,5.0,1.0,2.0,4.0,5.0,15.0,2.0,4.0,1.0,4.0,1.0,2.0,4.0,1.0,2.0,4.0,5.0,6.0,6.0,4.0,3.0,3.0,4.0,5.0,1.0,2.0,4.0,5.0,2.0,3.0,3.0,2.0,1.0,4.0,5.0,6.0,1.0,3.0,4.0,2.0,4.0,2.0,1.0,414.0,3.0,13.0,4.0,1.0,3.0,5.0,53.0,12.0,1.0,2.0,4.0,5.0,1.0,3.0,1.0,2.0,2.0,6.0,4.0,3.0,5.0,3.0,2.0,1.0,5.0,124.0,5.0,4.0,23.0,5.0,5.0,1.0,2.0,5.0,4.0,23.0,4.0,5.0,2.0,3.0,4.0,23.0,4.0,23.0,23.0,23.0,2.0,3.0,33.0,43.0,43.0,34.0,12.0,43.0,21.0,123.0,12.0,32.0,32.0,32.0]

waveletHaar_packer_separate_testStub  :: IO [[Double]]
waveletHaar_packer_separate_testStub = do
  (read `fmap` readFile "./test/Data/haar_separate.tst" )


testWaveletHaar_PackerSeparate = dwt 12 haar wp_separate ys

compareWaveletHaarResults = do
  let rslt = testWaveletHaar_PackerSeparate
  ctrl <- waveletHaar_packer_separate_testStub
  return $ (length rslt ) == (length ctrl)


main :: IO ()
main = do
  haar_separate_test_data <- waveletHaar_packer_separate_testStub 
  hspec $ spec

-- | Have to bring in test data from a file to test this  
spec :: Spec
spec  = do
  describe "absAvgItrFcn" $ do
    it "should return the average when folded over a series" $ do 
      let tstData = replicate 10 1.3
          (avg,tot) = foldl (\(a,t) b -> absAvgItrFcn a b (t+1) ) (0,0) tstData
      avg `shouldBe` 1.3
  describe "absMaxItrFcn" $ do
    it "should return the maximum when folded overa  series" $ do 
      let tstData = (replicate 10 1.3) ++ [100] :: [Double] 
          mx = foldl (\a b -> absMaxItrFcn a b )  0 tstData
      mx `shouldBe` 100
  describe "minMaxAndAverage" $ do
    it "should return the average, max , length of a given vector of doubles" $ do 
      let tstData = testVectorData
          (mn,mx,av,tot) = minMaxAndAverage tstData
      mx `shouldBe` 100


testVectorData = ys

testReconstructTimeSeries  = reconstructTimeSeries (w - n) haar wp_separate $ drop n testWaveletHaar_PackerSeparate
  where n = 0
        w = 12



--OSF (SeriesFactors {seriesMin = 1.0, seriesMax = 100.0, seriesMean = 1.099, seriesCount = 1000})
testComputeOldSeriesmatrix = OSF . computeSeriesFactors $ testVectorData


-- NSF (SeriesFactors {seriesMin = 2.8284271247461934, seriesMax = 37.830212793480314, seriesMean = 3.108441410096067, seriesCount = 125})
testComputeNewSeriesmatrix = NSF . computeSeriesFactors $ testReconstructTimeSeries


-- SM (2><1)
--  [ -7.374250048383981
--  , 2.8382461515606137 ]
testComputeScalingMatrix = computeScalingMatrix testComputeNewSeriesmatrix testComputeOldSeriesmatrix


testApplyScalingMatrix = applyScalingMatrix testComputeScalingMatrix testReconstructTimeSeries

testGetNewScalingFactors = computeSeriesFactors testApplyScalingMatrix
