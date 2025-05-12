module Torrent.PeerAddress
  ( PeerAddress (..)
  , parsePeerAddress
  , bsToPeerAddress
  ) where

import Data.ByteString qualified as BS
import Control.Monad (void)
import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as A
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4

data PeerAddress = PeerAddress
  { host :: IPv4
  , port :: Int
  }

instance Show PeerAddress where
  show PeerAddress {..} = IPv4.encodeString host <> ":" <> show port

parsePeerAddress :: A.Parser PeerAddress
parsePeerAddress =
  ( do
      host <- IPv4.parserUtf8
      void $ A.char ':'
      port <- A.decimal
      pure PeerAddress {..}
  )
    <?> "peer address"

bsToPeerAddress :: BS.ByteString -> PeerAddress
bsToPeerAddress bs
  | [b1, b2, b3, b4, b5, b6] <- BS.unpack bs =
      let host = IPv4.ipv4 b1 b2 b3 b4
          port = fromIntegral b5 * 256 + fromIntegral b6
      in  PeerAddress {..}
  | otherwise = error "invalid input to `bsToPeer`"
