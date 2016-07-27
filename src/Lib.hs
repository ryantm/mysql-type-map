{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , ColumnType(..)
    , Signed(..)
    , Zerofill(..)
    , National(..)
    ) where

import Data.Text
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import Text.Parsec (runParserT)
import Data.Functor.Identity (runIdentity)
import Control.Applicative ((<|>))

data Signed = Signed | Unsigned
  deriving (Eq, Show)

data Zerofill = NoZerofill | Zerofill
  deriving (Eq, Show)

data National = NotNational | National
  deriving (Eq, Show)

data ColumnType =
    Bit Integer
  | TinyInt Integer Signed Zerofill
  | SmallInt Integer Signed Zerofill
  | MediumInt Integer Signed Zerofill
  | MyInt Integer Signed Zerofill
  | BigInt Integer Signed Zerofill
  | Decimal Integer Integer Signed Zerofill
  | MyFloat (Maybe Integer) (Maybe Integer) Signed Zerofill
  | MyDouble (Maybe Integer) (Maybe Integer) Signed Zerofill
  | Date
  | DateTime Integer
  | Timestamp Integer
  | Time Integer
  | Year Integer
  | MyChar National Integer (Maybe Text) (Maybe Text)
  | VarChar National Integer (Maybe Text) (Maybe Text)
  | Binary Integer
  | VarBinary Integer
  | TinyBlob
  | MyText (Maybe Integer) (Maybe Text) (Maybe Text)
  | MediumBlob
  deriving (Eq, Show)

parse :: Text -> ColumnType
parse t =
  let f = runIdentity (runParserT parseTypes () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b

tryEof :: (TokenParsing m, Monad m) => m a -> m a
tryEof p = try (p <* eof)

parseTypes :: (TokenParsing m, Monad m) => m ColumnType
parseTypes =
  tryEof bit
  <|> tryEof tinyInt
  <|> tryEof bool
  <|> tryEof smallInt
  <|> tryEof mediumInt
  <|> tryEof myInt
  <|> tryEof myInteger
  <|> tryEof bigInt
  <|> tryEof myDecimal
  <|> tryEof myFloat
  <|> myDouble
  <|> tryEof pFloat
  <|> tryEof date
  <|> tryEof dateTime
  <|> tryEof timestamp
  <|> tryEof time
  <|> tryEof year
  <|> tryEof char
  <|> tryEof varchar
  <|> tryEof binary
  <|> tryEof varbinary
  <|> tryEof tinyBlob
  <|> tryEof myText
  <|> tryEof mediumBlob

bit :: TokenParsing m => m ColumnType
bit =
  textSymbol "BIT" *>
  option (Bit 1) (Bit <$> parens integer)

tinyInt :: TokenParsing f => f ColumnType
tinyInt = mType "TINYINT" TinyInt

bool :: TokenParsing f => f ColumnType
bool = textSymbol "BOOL" *>
  skipOptional (textSymbol "EAN") *>
  pure (TinyInt 1 Signed NoZerofill)

smallInt :: TokenParsing f => f ColumnType
smallInt = mType "SMALLINT" SmallInt

mediumInt :: TokenParsing f => f ColumnType
mediumInt = mType "MEDIUMINT" MediumInt

myInt :: TokenParsing f => f ColumnType
myInt = mType "INT" MyInt

myInteger :: TokenParsing f => f ColumnType
myInteger = mType "INTEGER" MyInt

bigInt :: TokenParsing f => f ColumnType
bigInt = mType "BIGINT" BigInt

unsigned :: TokenParsing f => f Signed
unsigned = option Signed (textSymbol "UNSIGNED" *> pure Unsigned)

zerofill :: TokenParsing f => f Zerofill
zerofill = option NoZerofill (textSymbol "ZEROFILL" *> pure Zerofill)


mType
  :: TokenParsing f =>
     Text
  -> (Integer -> Signed -> Zerofill -> ColumnType)
  -> f ColumnType
mType n c =
  c <$> (
  textSymbol n *>
  option 1 (parens integer)) <*> unsigned <*> zerofill

myDecimal :: TokenParsing f => f ColumnType
myDecimal = uncurry Decimal <$> (
  (try (textSymbol "DECIMAL")
  <|> textSymbol "DEC"
  <|> textSymbol "NUMERIC"
  <|> textSymbol "FIXED")
  *>
  option (10,0) (
      parens (
          (,) <$>
          integer <*>
          option 0 (comma *>
                    integer)))) <*> unsigned <*> zerofill

myFloat :: TokenParsing f => f ColumnType
myFloat = myFloatOrDouble "FLOAT" MyFloat

myDouble :: (TokenParsing f, Monad f) => f ColumnType
myDouble =
  tryEof (myFloatOrDouble "DOUBLE" MyDouble)
  <|> tryEof (myFloatOrDouble "DOUBLE PRECISION" MyDouble)
  <|> tryEof (myFloatOrDouble "REAL" MyDouble)

myFloatOrDouble :: TokenParsing f =>
     Text
  -> (Maybe Integer -> Maybe Integer -> Signed -> Zerofill -> ColumnType)
  -> f ColumnType
myFloatOrDouble n c = uncurry c <$> (
  textSymbol n
  *>
  option (Nothing,Nothing) (
      parens (
          (,) <$>
            (Just <$> integer) <*
            comma <*>
            (Just <$> integer)))) <*> unsigned <*> zerofill

pFloat :: (TokenParsing f, Monad f) => f ColumnType
pFloat = do
  textSymbol "FLOAT"
  i <- parens integer
  (if i < 25 then
     MyFloat Nothing Nothing
   else
     MyDouble Nothing Nothing) <$> unsigned <*> zerofill

date :: TokenParsing f => f ColumnType
date = textSymbol "DATE" *> pure Date

dateTime :: TokenParsing f => f ColumnType
dateTime = fspType "DATETIME" DateTime 0

timestamp :: TokenParsing f => f ColumnType
timestamp = fspType "TIMESTAMP" Timestamp 0

time :: TokenParsing f => f ColumnType
time = fspType "TIME" Time 0

year :: TokenParsing f => f ColumnType
year = fspType "YEAR" Year 4

fspType :: TokenParsing f =>
           Text
        -> (Integer -> ColumnType)
        -> Integer
        -> f ColumnType
fspType n c d =
  c <$> (
  textSymbol n *>
  option d (parens integer))

char :: (TokenParsing f, Monad f) => f ColumnType
char = MyChar <$>
  option NotNational (textSymbol "NATIONAL" *> pure National) <*
  textSymbol "CHAR" <*>
  option 1 (parens integer) <*>
  option Nothing (Just <$>
                   (try (textSymbol "CHARACTER SET") *> ident emptyIdents)) <*>
  option Nothing (Just <$> (try (textSymbol "COLLATE") *> ident emptyIdents))

varchar :: (TokenParsing f, Monad f) => f ColumnType
varchar = VarChar <$>
  option NotNational (textSymbol "NATIONAL" *> pure National) <*
  textSymbol "VARCHAR" <*>
  parens integer <*>
  option Nothing (Just <$>
                   (try (textSymbol "CHARACTER SET") *> ident emptyIdents)) <*>
  option Nothing (Just <$> (try (textSymbol "COLLATE") *> ident emptyIdents))

binary :: TokenParsing f => f ColumnType
binary = Binary <$> (textSymbol "BINARY" *> parens integer)

varbinary :: TokenParsing f => f ColumnType
varbinary = VarBinary <$> (textSymbol "VARBINARY" *> parens integer)

tinyBlob :: TokenParsing f => f ColumnType
tinyBlob = textSymbol "TINYBLOB" *> pure TinyBlob

myText :: (TokenParsing f, Monad f) => f ColumnType
myText = MyText <$>
  (textSymbol "TEXT" *>
  optional (parens integer)) <*>
  option Nothing (Just <$>
                   (try (textSymbol "CHARACTER SET") *> ident emptyIdents)) <*>
  option Nothing (Just <$> (try (textSymbol "COLLATE") *> ident emptyIdents))

mediumBlob :: TokenParsing f => f ColumnType
mediumBlob = textSymbol "MEDIUMBLOB" *> pure MediumBlob
