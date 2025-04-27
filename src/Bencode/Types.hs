module Bencode.Types
  ( Bencode (..)
  ) where

import Data.Aeson qualified as A
import Data.Text (Text)

data Bencode
  = BString Text
  | BInt Int
  | BArray
  | BDict
  deriving (Show)

instance A.ToJSON Bencode where
  toJSON (BString s) = A.String s
  toJSON (BInt n) = A.Number $ fromIntegral n
