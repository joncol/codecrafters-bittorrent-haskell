module Bencode.Parser
  ( parseBencode
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Text qualified as T

import Bencode.Types

parseBencode :: Parser Bencode
parseBencode = bInt <|> bString

bInt :: Parser Bencode
bInt = do
  void $ char 'i'
  n <- signed decimal
  void $ char 'e'
  pure $ BInt n

bString :: Parser Bencode
bString = do
  n <- decimal
  void $ char ':'
  str <- T.pack <$> count n anyChar
  pure $ BString str
