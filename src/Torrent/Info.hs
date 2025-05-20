module Torrent.Info
  ( TorrentInfo (..)
  , getTorrentInfo
  , Hash (..)
  , getPieceLength
  ) where

import Control.Monad.IO.Class
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Word (Word32)

import Bencode.Types
import Bencode.Util
import Torrent.Hash

data TorrentInfo = TorrentInfo
  { trackerUrl :: Text
  , fileLength :: Int64
  , infoHash :: Hash
  , pieceLength :: Word32
  , pieceHashes :: [Hash]
  }
  deriving (Show)

-- | Reads torrent information from a file.
-- TODO: Make this use proper error handling.
getTorrentInfo :: MonadIO m => FilePath -> m TorrentInfo
getTorrentInfo filename =
  do
    contents <- liftIO $ BS.readFile filename
    let decodedValue = Bin.decode $ BSL.fromStrict contents
        keyVals = case decodedValue of
          BDict keyVals' -> keyVals'
          _ -> error "torrent file is not a Bencoded dictionary"
        trackerUrl = case getDictValue "announce" keyVals of
          Just (BString url) -> BSE.decode BSE.latin1 url
          _ -> error "no tracker URL in torrent file"
        infoKeyVals = case getDictValue "info" keyVals of
          Just (BDict keyVals') -> keyVals'
          _ -> error "no info dictionary in torrent file"
        fileLength = case getDictValue "length" infoKeyVals of
          Just (BInt len') -> len'
          _ -> error "no length field in info dictionary"
        infoDictBS = BSL.toStrict . Bin.encode $ BDict infoKeyVals
        infoHash = Hash $ SHA1.hash infoDictBS
        pieceLength = case getDictValue "piece length" infoKeyVals of
          Just (BInt len') -> fromIntegral len'
          _ -> error "no piece length field in info dictionary"
        pieceHashes = case getDictValue "pieces" infoKeyVals of
          Just (BString s) -> map (Hash . BS.pack) . chunksOf 20 $ BS.unpack s
          _ -> error "no piece length field in info dictionary"
     in pure TorrentInfo {..}

getPieceLength :: TorrentInfo -> Int -> Int
getPieceLength torrentInfo pieceIndex
  | 0 <= pieceIndex && pieceIndex < fromIntegral wholePieceCount =
      fromIntegral torrentInfo.pieceLength
  | pieceIndex == pieceCount - 1 = lastPieceLength
  | otherwise = 0
  where
    wholePieceCount =
      torrentInfo.fileLength `div` fromIntegral torrentInfo.pieceLength
    lastPieceLength =
      fromIntegral $
        torrentInfo.fileLength `mod` fromIntegral torrentInfo.pieceLength
    pieceCount =
      ceiling $
        (fromIntegral torrentInfo.fileLength :: Double)
          / (fromIntegral torrentInfo.pieceLength :: Double)
