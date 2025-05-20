module Messages.Extensions.Metadata.Request
  ( Request (..)
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Word (Word8)
import Safe (lookupJust)

import Bencode.Types

data Request = Request
  { extensionMsgId :: Word8
  -- ^ This is the peer's metadata extension ID, which you received during the
  -- extension handshake.
  , pieceIndex :: Int64
  }
  deriving (Show)

msgId :: Word8
msgId = 20

msgType :: Integral a => a
msgType = 0

instance Binary Request where
  put Request {..} = do
    Bin.putWord32be $ fromIntegral (dictSize + 2)
    Bin.putWord8 msgId
    Bin.putWord8 extensionMsgId
    Bin.put dict
    where
      dict =
        BDict
          [ ("msg_type", BInt msgType)
          , ("piece", BInt pieceIndex)
          ]
      dictSize = BSL.length $ Bin.encode dict
  get = do
    len <- Bin.getWord32be
    Bin.isolate (fromIntegral len) $ do
      msgId' <- Bin.getWord8
      guard $ msgId' == msgId
      extensionMsgId <- Bin.getWord8
      dict :: Bencode <- Bin.get
      case dict of
        BDict keyVals -> do
          let pieceIndexBencode = lookupJust "piece" keyVals
          case pieceIndexBencode of
            BInt pieceIndex -> pure Request {..}
            _ ->
              error
                "expected `piece` to be a BInt in response to \
                \metadata extension request"
        _ ->
          error
            "expected a BDict in response to metadata extension request"
