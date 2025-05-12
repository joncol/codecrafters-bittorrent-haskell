module Messages.PeerHandshake
  ( PeerHandshakeResponse (..)
  , doHandshake
  ) where

import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Net.IPv4 qualified as IPv4
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as NS
import Network.Socket.ByteString qualified as N

import Torrent.Hash
import Torrent.Info
import Torrent.PeerAddress

data PeerHandshakeResponse = PeerHandshakeResponse
  { infoHash :: Hash
  , peerId :: BS.ByteString
  }
  deriving (Show)

instance Binary PeerHandshakeResponse where
  put PeerHandshakeResponse {..} = do
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
    pure PeerHandshakeResponse {..}

doHandshake :: FilePath -> PeerAddress -> IO PeerHandshakeResponse
doHandshake filename (PeerAddress {host, port}) = do
  NS.connect (IPv4.encodeString host) (show port) $ \(socket, _addr) -> do
    sendHandshakeMessage socket =<< getTorrentInfo filename
    recvHandshakeResponse socket
  where
    sendHandshakeMessage :: Socket -> TorrentInfo -> IO ()
    sendHandshakeMessage socket torrentInfo =
      NS.send socket . BSL.toStrict . Bin.encode $
        PeerHandshakeResponse
          { infoHash = torrentInfo.infoHash
          , peerId = BS.replicate 20 0 -- can be anything
          }

    recvHandshakeResponse :: Socket -> IO PeerHandshakeResponse
    recvHandshakeResponse socket =
      Bin.decode . BSL.fromStrict <$> N.recv socket 4096
