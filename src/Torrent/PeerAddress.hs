module Torrent.PeerAddress
  ( PeerAddress (..)
  , parsePeerAddress
  ) where

import Control.Monad (void)
import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as A
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4

data PeerAddress = PeerAddress
  { ip :: IPv4
  , port :: Int
  }
  deriving (Show)

parsePeerAddress :: A.Parser PeerAddress
parsePeerAddress =
  ( do
      ip <- IPv4.parserUtf8
      void $ A.char ':'
      port <- A.decimal
      pure PeerAddress {..}
  )
    <?> "peer address"
