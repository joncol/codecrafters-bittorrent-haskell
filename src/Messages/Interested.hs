module Messages.Interested
  ( Interested (..)
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin

data Interested = Interested deriving (Show)

msgId :: Bin.Word8
msgId = 2

instance Binary Interested where
  put Interested = do
    Bin.putWord32be 1
    Bin.putWord8 msgId
  get = do
    msgLen <- Bin.getWord32be
    guard $ msgLen == 1
    msgId' <- Bin.getWord8
    guard $ msgId' == msgId
    pure Interested
