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
  , testProperty "TINYINT[(m)] [UNSIGNED] [ZEROFILL] gives TinyInt" $
    mSZ "TINYINT" TinyInt
  , testCase "BOOL gives TinyInt 1 Signed NoZerofill" $
    parse "BOOL" @?= TinyInt 1 Signed NoZerofill
  , testCase "BOOLEAN gives TinyInt 1 Signed NoZerofill" $
    parse "BOOLEAN" @?= TinyInt 1 Signed NoZerofill
  , testProperty "SMALLINT[(m)] [UNSIGNED] [ZEROFILL] gives SmallInt" $
    mSZ "SMALLINT" SmallInt
  , testProperty "MEDIUMINT[(m)] [UNSIGNED] [ZEROFILL] gives MediumInt" $
    mSZ "MEDIUMINT" MediumInt
  , testProperty "INT[(m)] [UNSIGNED] [ZEROFILL] gives MyInt" $
    mSZ "INT" MyInt
  , testProperty "INTEGER[(m)] [UNSIGNED] [ZEROFILL] gives MyInt" $
    mSZ "INTEGER" MyInt
  , testProperty "BIGINT[(m)] [UNSIGNED] [ZEROFILL] gives BigInt" $
    mSZ "BIGINT" BigInt
  -- TODO? SERIAL
  , testProperty "DECIMAL[(m,[d])] [UNSIGNED] [ZEROFILL] gives Decimal" $
    decimalProp "DECIMAL"
  , testProperty "DEC[(m,[d])] [UNSIGNED] [ZEROFILL] gives Decimal" $
    decimalProp "DEC"
  , testProperty "NUMERIC[(m,[d])] [UNSIGNED] [ZEROFILL] gives Decimal" $
    decimalProp "NUMERIC"
  , testProperty "FIXED[(m,[d])] [UNSIGNED] [ZEROFILL] gives Decimal" $
    decimalProp "FIXED"
  , testProperty "FLOAT[(m,d)] [UNSIGNED] [ZEROFILL] gives MyFloat" $
    floatProp
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

decimalProp n m d s z =
  let sS = case s of
        Signed -> ""
        Unsigned -> "UNSIGNED"
      zS = case z of
        NoZerofill -> ""
        Zerofill -> "ZEROFILL"
      mdS = case m of
        Nothing -> ""
        Just i ->
          case d of
            Nothing -> "(" ++ show i ++ ")"
            Just di -> "(" ++ show i ++ "," ++ show di ++ ")"
      mR = case m of
        Nothing -> 10
        Just i -> i
      dR = case d of
        Nothing -> 0
        Just i -> case m of
          Nothing -> 0
          Just _ -> i
      t = n ++ " " ++ mdS ++ " " ++ sS ++ " " ++ zS
      in
        parse (pack t) == Decimal mR dR s z

floatProp md s z =
  let sS = case s of
        Signed -> ""
        Unsigned -> "UNSIGNED"
      zS = case z of
        NoZerofill -> ""
        Zerofill -> "ZEROFILL"
      mdS = case md of
        Nothing -> ""
        Just (m, d) ->"(" ++ show m ++ "," ++ show d ++ ")"
      (mR, dR) = case md of
        Nothing -> (Nothing, Nothing)
        Just (m, d) -> (Just m, Just d)
      t = "FLOAT " ++ mdS ++ " " ++ sS ++ " " ++ zS
      in
        parse (pack t) == MyFloat mR dR s z
