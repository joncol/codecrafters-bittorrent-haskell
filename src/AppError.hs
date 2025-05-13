module AppError
  ( AppError (..)
  ) where

data AppError
  = NoPeersInTorrentFile
  | InvalidHandshakeResponse
  deriving (Show)
