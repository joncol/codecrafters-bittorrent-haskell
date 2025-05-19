module AppError
  ( AppError (..)
  ) where

import Data.Text (Text)

data AppError
  = NoPeersInTorrentFile
  | InvalidHandshakeResponse
  | DecodingError Text
  deriving (Show)
