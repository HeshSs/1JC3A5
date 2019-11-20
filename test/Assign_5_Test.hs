{- Assignment 5 Tests
 - Name: TODO add full name
 - Date: TODO add of completion
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck prop1
          -- TODO implement real tests

prop1 :: Int -> Bool
prop1 _ = True
