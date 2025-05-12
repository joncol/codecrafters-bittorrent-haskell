module Messages.DownloadPiece
  ( downloadPiece
  ) where

import Control.Monad.IO.Class (MonadIO)
import Network.Simple.TCP (Socket)

import Torrent.PeerAddress

downloadPiece :: MonadIO m => Socket -> FilePath -> Int -> m _
downloadPiece socket outputFilename pieceIndex = _
