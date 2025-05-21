module Messages.Piece
  ( Piece (..)
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.ByteString qualified as BS
import Data.Word (Word8)
import GHC.Generics (Generic)

data Piece = Piece
  { index :: Bin.Word32
  , begin :: Bin.Word32
  , block :: BS.ByteString
  , mid :: Word8
  , msgLen :: Int
  }
  deriving (Generic, Show)

msgId :: Bin.Word8
msgId = 7

instance Binary Piece where
  get = do
    msgLen <- fromIntegral <$> Bin.getWord32be
    Bin.isolate msgLen $ do
      msgId' <- Bin.getWord8
      guard $ msgId' == msgId
      index <- Bin.getWord32be
      begin <- Bin.getWord32be
      let blockLen = msgLen - 9
      block <- Bin.getByteString blockLen
      pure Piece {mid = msgId', ..}
