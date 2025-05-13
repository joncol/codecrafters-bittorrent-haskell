module Messages.Request
  ( Request (..)
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin

data Request = Request
  { index :: Bin.Word32
  , begin :: Bin.Word32
  , len :: Bin.Word32
  }
  deriving (Show)

msgId :: Bin.Word8
msgId = 6

instance Binary Request where
  put Request {..} = do
    Bin.putWord32be 13
    Bin.putWord8 msgId
    Bin.putWord32be index
    Bin.putWord32be begin
    Bin.putWord32be len
  get = do
    _msgLen <- Bin.getWord32be
    msgId' <- Bin.getWord8
    guard $ msgId' == msgId
    index <- Bin.getWord32be
    begin <- Bin.getWord32be
    len <- Bin.getWord32be
    pure Request {..}
