module Messages.PeerHandshake
  ( PeerHandshake (..)
  ) where

import Control.Monad (replicateM, replicateM_)
import Data.Binary
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
    putWord8 . fromIntegral $ BS.length protocolString
    putByteString protocolString
    putByteString $ BS.replicate 8 0 -- reserved bytes
    putByteString $ getHash infoHash
    putByteString peerId
    where
      protocolString :: BS.ByteString
      protocolString = "BitTorrent protocol"
  get = do
    replicateM_ 28 getWord8
    infoHash <- Hash . BS.pack <$> replicateM 20 getWord8
    peerId <- BS.pack <$> replicateM 20 getWord8
    pure PeerHandshake {..}
