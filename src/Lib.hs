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
  deriving (Eq, Show)

parse :: Text -> ColumnType
parse t =
  let f = runIdentity (runParserT parseTypes () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b

parseTypes :: TokenParsing m => m ColumnType
parseTypes = bit <|> tinyInt


bit :: TokenParsing m => m ColumnType
bit =
  textSymbol "BIT" *>
  option (Bit 1) (Bit <$> parens integer)

tinyInt :: TokenParsing m => m ColumnType
tinyInt = TinyInt <$> (
  textSymbol "TINYINT" *>
  option 1 (parens integer)) <*>
  option Signed (textSymbol "UNSIGNED" *> pure Unsigned) <*>
  option NoZerofill (textSymbol "ZEROFILL" *> pure Zerofill)
