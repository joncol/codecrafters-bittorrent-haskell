module AppError
  ( AppError (..)
  ) where

data AppError = NoPeersInTorrentFile
  deriving (Show)
