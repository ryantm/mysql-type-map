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
    testCase "BIT gives Bit 1" $
      parse (pack ("BIT")) @?= Bit 1
  , testProperty "BIT (m) gives Bit m" $
    \ m ->
      (parse (pack ("BIT (" ++ show m ++ ")"))) == Bit (m :: Integer)
  , testProperty "TINYINT (m) gives TinyInt m Signed NoZerofill" $
    \ m ->
      (parse (pack ("TINYINT (" ++ show m ++ ")"))) == TinyInt (m :: Integer) Signed NoZerofill
  ]
