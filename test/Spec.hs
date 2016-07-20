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
    mSZ "TINYINT" TinyInt
  , testProperty "SMALLINT [(m)] [UNSIGNED] [ZEROFILL] gives SmallInt" $
    mSZ "SMALLINT" SmallInt
  , testProperty "MEDIUMINT [(m)] [UNSIGNED] [ZEROFILL] gives MediumInt" $
    mSZ "MEDIUMINT" MediumInt
  ]

mSZ n c m s z =
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
      t = n ++ " " ++ mS ++ " " ++ sS ++ " " ++ zS
      in
        parse (pack t) == c mR s z
