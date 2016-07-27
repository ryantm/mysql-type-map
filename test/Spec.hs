{-# Language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lib
import Data.Text
import Data.Monoid

main :: IO ()
main = defaultMain tests


instance Arbitrary Signed where
  arbitrary = elements [Signed, Unsigned]

instance Arbitrary Zerofill where
  arbitrary = elements [NoZerofill, Zerofill]

tests :: TestTree
tests = testGroup "Tests" [numericTests, dateTimeTests, stringTests]

numericTests :: TestTree
numericTests = testGroup "Numeric" [
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
    floatOrDoubleProp "FLOAT" MyFloat
  , testProperty "DOUBLE[(m,d)] [UNSIGNED] [ZEROFILL] gives MyFloat" $
    floatOrDoubleProp "DOUBLE" MyDouble
  , testProperty "DOUBLE PRECISION[(m,d)] [UNSIGNED] [ZEROFILL] gives MyFloat" $
    floatOrDoubleProp "DOUBLE PRECISION" MyDouble
  , testProperty "REAL[(m,d)] [UNSIGNED] [ZEROFILL] gives MyFloat" $
    floatOrDoubleProp "REAL" MyDouble
  -- TODO: Handle REAL_AS_FLOAT option
  , testProperty "FLOAT(p) [UNSIGNED] [ZEROFILL] gives MyFloat or Double" $
    pFloatProp
  ]

dateTimeTests :: TestTree
dateTimeTests = testGroup "Date/Time" [
    testCase "DATE gives Date" $
      parse "DATE" @?= Date
  , testProperty "DATETIME[(fsp)] gives DateTime fsp" $
    fspProp "DATETIME" DateTime 0
  , testProperty "TIMESTAMP[(fsp)] gives Timestamp fsp" $
    fspProp "TIMESTAMP" Timestamp 0
  , testProperty "TIME[(fsp)] gives Time fsp" $
    fspProp "TIME" Time 0
  , testProperty "YEAR[(4)] gives Time 4" $
    fspProp "YEAR" Year 4
  ]

stringTests :: TestTree
stringTests = testGroup "String" [
    testProperty
      "[NATIONAL] CHAR[(M)] [CHARACTER SET charset_name] [COLLATE collation_name]"
      charProp
  , testProperty
      "[NATIONAL] VARCHAR(M) [CHARACTER SET charset_name] [COLLATE collation_name]"
      varcharProp
  , testProperty "BINARY(M)" $
      \ m ->
        parse ("BINARY(" <> pack (show m) <> ")") == Binary m
  ,  testProperty "VARBINARY(M)" $
       \ m ->
       parse ("VARBINARY(" <> pack (show m) <> ")") == VarBinary m
  ,  testCase "TINYBLOB" $ parse "TINYBLOB" @?= TinyBlob
  , testProperty
    "TEXT[(M)] [CHARACTER SET charset_name] [COLLATE collation_name]"
    textProp
  ,  testCase "MEDIUMBLOB" $ parse "MEDIUMBLOB" @?= MediumBlob
  , testProperty
    "MEDIUMTEXT [CHARACTER SET charset_name] [COLLATE collation_name]"
    mediumTextProp
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

floatOrDoubleProp n c md s z =
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
      t = n ++ " " ++ mdS ++ " " ++ sS ++ " " ++ zS
  in
    parse (pack t) == c mR dR s z

pFloatProp p s z =
  let sS = case s of
        Signed -> ""
        Unsigned -> "UNSIGNED"
      zS = case z of
        NoZerofill -> ""
        Zerofill -> "ZEROFILL"
      c = if (p :: Integer) < 25 then  -- 0 to 24 Float, 25 to 53 Double
            MyFloat
          else
            MyDouble
      t = "FLOAT(" ++ show p ++ ")" ++ " " ++ sS ++ " " ++ zS
  in
    parse (pack t) == c Nothing Nothing s z

fspProp n c d fsp =
  let fspS = case fsp of
        Nothing -> ""
        Just i -> "(" <> pack (show i) <> ")"
      fspR = case fsp of
        Nothing -> d
        Just i -> i
      t = n <> fspS
  in
    parse t == c fspR

data NonSpace = NonSpace Text
  deriving (Eq, Show)

instance Arbitrary NonSpace where
  arbitrary = do
    c1 <- elements (['A'..'Z'] <> ['a'..'z'] <> ['_'])
    rest <- listOf (elements (['A'..'Z'] <> ['a'..'z'] <> ['_'] <> ['0'..'9']))
    return (NonSpace (pack (c1 : rest)))

instance Arbitrary National where
  arbitrary = elements [NotNational, National]

charProp national m charset collation =
  let natS = case national of
        NotNational -> ""
        National -> "NATIONAL "
      mS = case m of
        Nothing -> ""
        Just i -> "(" <> pack (show i) <> ")"
      mR = case m of
        Nothing -> 1
        Just i -> i
      charsetS = case charset of
        Nothing -> ""
        Just (NonSpace c) -> "CHARACTER SET " <> c
      charsetR = case charset of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      collationS = case collation of
        Nothing -> ""
        Just (NonSpace c) -> "COLLATE " <> c
      collationR = case collation of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      t = natS <> "CHAR" <> mS <> " " <> charsetS <> " " <> collationS
  in
    parse t == MyChar national mR charsetR collationR

varcharProp national m charset collation =
  let natS = case national of
        NotNational -> ""
        National -> "NATIONAL "
      mS = "(" <> pack (show m) <> ")"
      charsetS = case charset of
        Nothing -> ""
        Just (NonSpace c) -> "CHARACTER SET " <> c
      charsetR = case charset of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      collationS = case collation of
        Nothing -> ""
        Just (NonSpace c) -> "COLLATE " <> c
      collationR = case collation of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      t = natS <> "VARCHAR" <> mS <> " " <> charsetS <> " " <> collationS
  in
    parse t == VarChar national m charsetR collationR

textProp m charset collation =
  let mS = case m of
        Nothing -> ""
        Just i -> "(" <> pack (show i) <> ")"
      charsetS = case charset of
        Nothing -> ""
        Just (NonSpace c) -> "CHARACTER SET " <> c
      charsetR = case charset of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      collationS = case collation of
        Nothing -> ""
        Just (NonSpace c) -> "COLLATE " <> c
      collationR = case collation of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      t = "TEXT" <> mS <> " " <> charsetS <> " " <> collationS
  in
    parse t == MyText m charsetR collationR

mediumTextProp charset collation =
  let charsetS = case charset of
        Nothing -> ""
        Just (NonSpace c) -> "CHARACTER SET " <> c
      charsetR = case charset of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      collationS = case collation of
        Nothing -> ""
        Just (NonSpace c) -> "COLLATE " <> c
      collationR = case collation of
        Nothing -> Nothing
        Just (NonSpace c) -> Just c
      t = "MEDIUMTEXT" <> " " <> charsetS <> " " <> collationS
  in
    parse t == MediumText charsetR collationR
