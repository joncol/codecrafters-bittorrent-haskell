module Torrent.Peer
  ( Peer (..)
  , bsToPeer
  ) where

import Data.ByteString qualified as BS
import Net.IPv4

data Peer = Peer
  { host :: IPv4
  , port :: Int
  }

instance Show Peer where
  show Peer {..} = encodeString host <> ":" <> show port

bsToPeer :: BS.ByteString -> Peer
bsToPeer bs
  | [b1, b2, b3, b4, b5, b6] <- BS.unpack bs =
      let host = ipv4 b1 b2 b3 b4
          port = fromIntegral b5 * 256 + fromIntegral b6
      in  Peer {..}
  | otherwise = error "invalid input to `bsToPeer`"
