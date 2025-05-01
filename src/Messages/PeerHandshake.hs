module Messages.PeerHandshake
  ( PeerHandshake (..)
  , doHandshake
  ) where

import Control.Monad (replicateM, replicateM_)
import Data.Binary
import Data.Binary qualified as Bin
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Fmt
import Net.IPv4 qualified as IPv4
import Network.Simple.TCP qualified as NS
import Network.Socket.ByteString qualified as N

import Network.Simple.TCP (Socket)
import Torrent.Hash
import Torrent.Info
import Torrent.PeerAddress
import Util

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

doHandshake :: FilePath -> PeerAddress -> IO ()
doHandshake filename (PeerAddress {ip, port}) = do
  NS.connect (IPv4.encodeString ip) (show port) $ \(socket, _addr) -> do
    sendHandshakeMessage socket =<< getTorrentInfo filename
    handshakeResp <- recvHandshakeResponse socket

    fmtLn $ "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
  where
    sendHandshakeMessage :: Socket -> TorrentInfo -> IO ()
    sendHandshakeMessage socket torrentInfo =
      NS.send socket . BSL.toStrict . Bin.encode $
        PeerHandshake
          { infoHash = torrentInfo.infoHash
          , peerId = BS.replicate 20 1
          }

    recvHandshakeResponse :: Socket -> IO PeerHandshake
    recvHandshakeResponse socket =
      Bin.decode . BSL.fromStrict <$> N.recv socket 4096
