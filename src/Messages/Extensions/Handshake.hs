module Messages.Extensions.Handshake
  ( Handshake (..)
  ) where

import Control.Monad (guard)
import Data.Bifunctor (second)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word8)

import Bencode.Types

newtype Handshake = Handshake {extensions :: Map Text Word8}
  deriving (Show)

msgId :: Word8
msgId = 20

extensionMsgId :: Word8
extensionMsgId = 0

instance Binary Handshake where
  put Handshake {..} = do
    Bin.putWord32be $ fromIntegral (dictSize + 2)
    Bin.putWord8 msgId
    Bin.putWord8 extensionMsgId
    Bin.put dict
    where
      dict = BDict [("m", innerDict)]
      extensions' = map (second $ BInt . fromIntegral) $ Map.toList extensions
      innerDict = BDict extensions'
      dictSize = BSL.length $ Bin.encode dict
  get = do
    len <- Bin.getWord32be
    Bin.isolate (fromIntegral len) $ do
      msgId' <- Bin.getWord8
      guard $ msgId' == msgId
      extensionMsgId' <- Bin.getWord8
      guard $ extensionMsgId' == extensionMsgId
      dict :: Bencode <- Bin.get
      case dict of
        BDict keyVals ->
          let innerDict = lookup "m" keyVals
          in  pure Handshake {extensions = bDictToMap innerDict}
        _ -> error "expected a BDict in extension handshake response"

bDictToMap :: Maybe Bencode -> Map Text Word8
bDictToMap (Just (BDict keyVals)) = Map.fromList $ map (second toInt) keyVals
  where
    toInt (BInt n) = fromIntegral n
    toInt val = error $ "expected a BInt, was " <> show val
bDictToMap _ = error "expected a BDict"
