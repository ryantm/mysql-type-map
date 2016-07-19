{-# Language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lib
import Data.Text

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    testCase "1=1" $
    1 @?= 1
  , testProperty "BIT (m) gives Bit m" $
    \ m ->
      (parse (pack ("BIT (" ++ show m ++ ")"))) == Bit (m :: Integer)
  ]
