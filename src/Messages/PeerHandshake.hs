module Messages.PeerHandshake
  ( PeerHandshake (..)
  , doHandshake
  ) where

import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
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
    Bin.putWord8 . fromIntegral $ BS.length protocolString
    putByteString protocolString
    putByteString $ BS.replicate 8 0 -- reserved bytes
    putByteString $ getHash infoHash
    putByteString peerId
    where
      protocolString :: BS.ByteString
      protocolString = "BitTorrent protocol"
  get = Bin.isolate (28 + 20 + 20) $ do
    Bin.skip 28
    infoHash <- Hash <$> Bin.getByteString 20
    peerId <- Bin.getByteString 20
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
          , peerId = BS.replicate 20 0 -- can be anything
          }

    recvHandshakeResponse :: Socket -> IO PeerHandshake
    recvHandshakeResponse socket =
      Bin.decode . BSL.fromStrict <$> N.recv socket 4096
