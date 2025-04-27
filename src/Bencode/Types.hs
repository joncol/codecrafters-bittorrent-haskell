module Bencode.Types
  ( Bencode (..)
  ) where

import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Vector qualified as V

data Bencode
  = BString Text
  | BInt Int
  | BList [Bencode]
  | BDict
  deriving (Show)

instance A.ToJSON Bencode where
  toJSON (BString s) = A.String s
  toJSON (BInt n) = A.Number $ fromIntegral n
  toJSON (BList xs) = A.Array . V.fromList $ map A.toJSON xs
