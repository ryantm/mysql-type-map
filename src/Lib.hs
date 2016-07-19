{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , Bit(..)
    ) where

import Data.Text
import Text.Parser.Token
import Text.Parsec (runParserT)
import Data.Functor.Identity (runIdentity)

data Bit = Bit Integer
  deriving (Eq, Show)

parse :: Text -> Bit
parse t =
  let f = runIdentity (runParserT bit () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b


bit :: (TokenParsing m, Monad m) => m Bit
bit = do
  textSymbol "BIT"
  i <- parens integer
  return (Bit i)
