{- Assignment 5 Tests
 - Name: Hishmat Salehi
 - Date: 29/11/2019
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test propDefiniteIntegral: "
          quickCheck propDefiniteIntegral
          print "Performing Test propfunH: "
          quickCheck propfunH
          print "Performing Test propfunK: "
          quickCheck propfunK

propDefiniteIntegral :: Double -> Double -> (Double -> Double) -> Bool
propDefiniteIntegral a b g = definiteIntegral a b g 1000 <= definiteIntegral a b g 100

propfunH :: Integer -> Bool
propfunH n = funH n <= funH (n*10)

propfunK :: Double -> Bool
propfunK n = funK n <=  funH (n*100)