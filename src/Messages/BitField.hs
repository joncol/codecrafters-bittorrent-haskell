module Messages.BitField
  ( BitField (..)
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import GHC.Generics (Generic)

data BitField = BitField {messageId :: Bin.Word8} deriving (Generic, Show)

msgId :: Bin.Word8
msgId = 5

instance Binary BitField where
  get = do
    len <- Bin.getWord32be
    msgId' <- Bin.getWord8
    guard $ msgId' == msgId
    Bin.skip $ fromIntegral (len - 1) -- Ignore the message for now.
    pure BitField {messageId = msgId'}
