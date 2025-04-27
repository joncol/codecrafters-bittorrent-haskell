module Bencode.Parser
  ( parseBencode
  ) where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Text qualified as T

import Bencode.Types

parseBencode :: Parser Bencode
parseBencode = bInt <|> bString <|> bList

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

bList :: Parser Bencode
bList = do
  void $ char 'l'
  xs <- many parseBencode
  void $ char 'e'
  pure $ BList xs
