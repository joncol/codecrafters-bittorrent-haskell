module Bencode.Parser
  ( parseBencodeValue
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Encoding qualified as BSE

import Bencode.Types

parseBencodeValue :: Parser Bencode
parseBencodeValue = bInt <|> bString <|> bList <|> bDict

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
  str <- BS8.pack <$> count n anyChar
  pure $ BString str

bList :: Parser Bencode
bList = do
  void $ char 'l'
  xs <- many parseBencodeValue
  void $ char 'e'
  pure $ BList xs

bDict :: Parser Bencode
bDict = do
  void $ char 'd'
  keyVals <- many $ do
    BString key <- bString
    value <- parseBencodeValue
    pure $ (BSE.decode BSE.latin1 key, value)
  void $ char 'e'
  pure $ BDict keyVals
