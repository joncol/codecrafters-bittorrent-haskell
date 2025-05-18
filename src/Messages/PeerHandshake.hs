module Messages.PeerHandshake
  ( PeerHandshake (..)
  ) where

import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS

import Torrent.Hash

data PeerHandshake = PeerHandshake
  { infoHash :: Hash
  , peerId :: BS.ByteString
  }
  deriving (Show)

instance Binary PeerHandshake where
  put PeerHandshake {..} = do
    Bin.putWord8 . fromIntegral $ BS.length protocolString
    putByteString protocolString
    putByteString $ BS.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00]
    putByteString $ getHash infoHash
    putByteString peerId
    where
      protocolString :: BS.ByteString
      protocolString = "BitTorrent protocol"
  get = Bin.isolate (28 + 20 + 20) $ do
    Bin.skip 28
    infoHash <- Hash <$> Bin.getByteString 20
    peerId <- Bin.getByteString 20
    pure PeerHandshake {..}
