module Bencode.Util (
  getDictValue
) where

import Data.Foldable qualified as Foldable
import Data.Text (Text)

import Bencode.Types

getDictValue :: Text -> [(Text, Bencode)] ->  Maybe Bencode
getDictValue key keyVals =
  snd <$> Foldable.find (\(key', _) -> key' == key) keyVals
