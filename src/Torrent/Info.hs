module Torrent.Info
  ( TorrentInfo (..)
  , getTorrentInfo
  , Hash (..)
  ) where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.Attoparsec.ByteString
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BSB16
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Text qualified as T

import Bencode.Parser
import Bencode.Types
import Bencode.Util

data TorrentInfo = TorrentInfo
  { trackerUrl :: Text
  , length :: Int64
  , infoHash :: Hash
  , pieceLength :: Int64
  , pieceHashes :: [Hash]
  }
  deriving (Show)

newtype Hash = Hash {getHash :: BS.ByteString}

instance Show Hash where
  show (Hash bs) = T.unpack . BSE.decode BSE.latin1 $ BSB16.encode bs

getTorrentInfo :: FilePath -> IO TorrentInfo
getTorrentInfo filename =
  do
    contents <- BS.readFile filename
    let decodedValue =
          fromRight (error "parse error") $
            parseOnly parseBencodeValue contents
        keyVals = case decodedValue of
          BDict keyVals' -> keyVals'
          _ -> error "torrent file is not a Bencoded dictionary"
        trackerUrl = case getDictValue "announce" keyVals of
          Just (BString url) -> BSE.decode BSE.latin1 url
          _ -> error "no tracker URL in torrent file"
        infoKeyVals = case getDictValue "info" keyVals of
          Just (BDict keyVals') -> keyVals'
          _ -> error "no info dictionary in torrent file"
        len = case getDictValue "length" infoKeyVals of
          Just (BInt len') -> len'
          _ -> error "no length field in info dictionary"
        infoDictBS = LBS.toStrict . Bin.encode $ BDict infoKeyVals
        infoHash = Hash $ SHA1.hash infoDictBS
        pieceLength = case getDictValue "piece length" infoKeyVals of
          Just (BInt len') -> len'
          _ -> error "no piece length field in info dictionary"
        pieceHashes = case getDictValue "pieces" infoKeyVals of
          Just (BString s) -> map (Hash . BS.pack) . chunksOf 20 $ BS.unpack s
          _ -> error "no piece length field in info dictionary"
     in pure TorrentInfo {length = len, ..}
