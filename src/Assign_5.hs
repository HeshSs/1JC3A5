{- Assignment 5
 - Name: Hishmat Salehi
 - Date: 29/11/2019
 - Trapezoidal Rule: https://en.wikipedia.org/wiki/Trapezoidal_rule
 -}
module Assign_5 where

macid :: String
macid = "salehh6"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: The definiteIntegral function approximates the region under the 
 - graph of the function f(x) as a trapezoid and calculating it's area by dividing the interval a to b into small intervals using n.
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = if n == 0 then 0
                                     else if a == b then 0
                                                    else let interval = (b - a) / fromInteger n
                                                             smallInterval = ((g b) + (g (b - interval))) / 2
                                                         in (interval * smallInterval) + (definiteIntegral a (b - interval) g (n - 1))

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: The function funH calculates the integral between the functions x^(1/n) 
 - and x^n using the the function definiteIntegral and n slices between 0 and 1 
 -}
funH :: Integer -> Double
funH n = if n <= 0 then 0
                   else (definiteIntegral 0 1 nroot n) - (definiteIntegral 0 1 npower n)
                        where nroot = (\x -> x ** (1/fromInteger n))
                              npower = (\x -> x ** (fromInteger n))
{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: The function funK calculates the integral of n**x using 100 small intervals from -1 to 1
 -}
funK :: Double -> Double
funK n = if n <= 0 then 0
                   else definiteIntegral (-1) 1 (\x -> n**x) 100
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: definiteIntegral 0 1 (\x -> x^3) 10000
 - - Expected Output: 0.25
 - - Actual Output: 0.25000000250003457
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral 0 10 (\x -> x) 10000
 - - Expected Output: 50.0
 - - Actual Output: 50.00000000000528
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral 0 10 (\x -> x^2) 10000
 - - Expected Output: 333.3333
 - - Actual Output: 333.3333350000631
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 1
 - - Input: funH 10
 - - Expected Output: 0.7768891981777205
 - - Actual Output: 0.7768891981777205
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 2
 - - Input: funH 100
 - - Expected Output: 0.9746897990239701
 - - Actual Output: 0.9746897990239701
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 3
 - - Input: funH 10000
 - - Expected Output: 0.9997418774993985
 - - Actual Output: 0.9997418774993985
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: funK 100
 - - Expected Output: 21.727899485314374
 - - Actual Output: 21.727899485314374
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 2
 - - Input: funK 10000
 - - Expected Output: 1088.804572469892
 - - Actual Output: 1088.804572469892
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 3
 - - Input: funK 1000000
 - - Expected Output: 72842.34574480829
 - - Actual Output: 72842.34574480829
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - QuickCheck Test Cases
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Property: 
        propDefiniteIntegral :: Double -> Double -> (Double -> Double) -> Bool
        propDefiniteIntegral a b g = definiteIntegral a b g 1000 <= definiteIntegral a b g 100
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: funH
 - - Property: 
        propfunH :: Integer -> Bool
        propfunH n = funH n <= funH (n*10)
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: funK
 - - Property: 
        propfunK :: Double -> Bool
        propfunK n = funK n <=  funH (n*100)
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 -}

