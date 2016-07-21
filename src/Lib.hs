{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , ColumnType(..)
    , Signed(..)
    , Zerofill(..)
    ) where

import Data.Text
import Text.Parser.Token
import Text.Parser.Combinators
import Text.Parsec (runParserT)
import Data.Functor.Identity (runIdentity)
import Control.Applicative ((<|>))

data Signed = Signed | Unsigned
  deriving (Eq, Show)
data Zerofill = NoZerofill | Zerofill
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
  deriving (Eq, Show)

parse :: Text -> ColumnType
parse t =
  let f = runIdentity (runParserT parseTypes () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b

parseTypes :: TokenParsing m => m ColumnType
parseTypes = try $
  try bit
  <|> try tinyInt
  <|> try bool
  <|> try smallInt
  <|> try mediumInt
  <|> try myInt
  <|> try myInteger
  <|> try bigInt
  <|> try myDecimal
  <|> try myFloat

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

mType
  :: TokenParsing f =>
     Text
  -> (Integer -> Signed -> Zerofill -> ColumnType)
  -> f ColumnType
mType n c =
  c <$> (
  textSymbol n *>
  option 1 (parens integer)) <*>
  option Signed (textSymbol "UNSIGNED" *> pure Unsigned) <*>
  option NoZerofill (textSymbol "ZEROFILL" *> pure Zerofill) <*
  eof

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
                    integer)))) <*>
  option Signed (textSymbol "UNSIGNED" *> pure Unsigned) <*>
  option NoZerofill (textSymbol "ZEROFILL" *> pure Zerofill) <*
  eof

myFloat :: TokenParsing f => f ColumnType
myFloat = uncurry MyFloat <$> (
  textSymbol "FLOAT"
  *>
  option (Nothing,Nothing) (
      parens (
          (,) <$>
            (Just <$> integer) <*
            comma <*>
            (Just <$> integer)))) <*>
  option Signed (textSymbol "UNSIGNED" *> pure Unsigned) <*>
  option NoZerofill (textSymbol "ZEROFILL" *> pure Zerofill) <*
  eof
