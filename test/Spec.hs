{-# Language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lib
import Data.Text

main :: IO ()
main = defaultMain tests


instance Arbitrary Signed where
  arbitrary = elements [Signed, Unsigned]

instance Arbitrary Zerofill where
  arbitrary = elements [NoZerofill, Zerofill]

tests :: TestTree
tests = testGroup "Tests" [
    testProperty "BIT [(m)] gives Bit" $
    \ m ->
      let mS = case m of
                 Nothing -> ""
                 Just i -> "(" ++ show i ++ ")"
          mR = case m of
                 Nothing -> 1
                 Just i -> i
          t = "BIT " ++ mS
      in
        parse (pack t) == Bit mR
  , testProperty "TINYINT [(m)] [UNSIGNED] [ZEROFILL] gives TinyInt" $
    \ m s z ->
      let sS = case s of
                 Signed -> ""
                 Unsigned -> "UNSIGNED"
          zS = case z of
                 NoZerofill -> ""
                 Zerofill -> "ZEROFILL"
          mS = case m of
                 Nothing -> ""
                 Just i -> "(" ++ show i ++ ")"
          mR = case m of
                 Nothing -> 1
                 Just i -> i
          t = "TINYINT " ++ mS ++ " " ++ sS ++ " " ++ zS
      in
        parse (pack t) == TinyInt mR s z
  ]
