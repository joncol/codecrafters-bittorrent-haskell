module Network
  ( getPeers
  , doHandshake
  , downloadPiece
  ) where

import Control.Monad.IO.Class
import Data.Attoparsec.ByteString (parseOnly)
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Fmt
import Net.IPv4 qualified as IPv4
import Network.HTTP.Client (Request (..))
import Network.HTTP.Req hiding (port)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as NS
import Network.Socket.ByteString qualified as N
import Text.URI

import Bencode.Parser
import Bencode.Types
import Bencode.Util
import Messages.BitField
import Messages.Interested
import Messages.PeerHandshake
import Messages.Piece
import Messages.Request
import Messages.Unchoke
import Torrent.Hash
import Torrent.Info
import Torrent.PeerAddress
import Util

bufferSize :: Integral n => n
bufferSize = 4096

-- | Helper function that decodes a message received from a socket.
recv :: Bin.Binary a => Socket -> IO a
recv socket = do
  -- Bin.decode . BSL.fromStrict <$> N.recv socket bufferSize
  bs <- N.recv socket bufferSize
  -- fmtLn $ "recv bs: " +| foldMap byteF (BS.unpack bs) |+ ""
  fmtLn $ "recv " +| BS.length bs |+ " bytes"
  pure . Bin.decode $ BSL.fromStrict bs

-- | Helper function that encodes and sends a message to a socket.
send :: Bin.Binary a => Socket -> a -> IO ()
send socket = NS.send socket . BSL.toStrict . Bin.encode

-- | Make a GET request to the torrent tracker to get a list of peers.
getPeers :: MonadIO m => Text -> TorrentInfo -> m [PeerAddress]
getPeers myPeerId torrentInfo = runReq defaultHttpConfig $ do
  uri <- mkURI torrentInfo.trackerUrl

  -- TODO: Is there a simpler way of handling the port?
  let port =
        (fromIntegral . fromMaybe 80)
          (either (const Nothing) (.authPort) (uri.uriAuthority))

  case useURI uri of
    Just (Left (url, _)) -> doReq url port
    Just (Right (url, _)) -> doReq url port
    Nothing -> error "invalid tracker URL"
  where
    doReq :: MonadHttp m => Url scheme -> Int -> m [PeerAddress]
    doReq url port = do
      resp <- reqCb GET url NoReqBody bsResponse (Req.port port) setQueryParams

      case parseOnly parseBencodeValue $ responseBody resp of
        Right (BDict keyVals) -> do
          let peersBStr = case getDictValue "peers" keyVals of
                Just (BString str) -> str
                _ -> error "tracker response did not contain `peers` field"
          pure . map bsToPeerAddress $ chunksOfBs 6 peersBStr
        _ -> error "could not parse response from tracker"

    setQueryParams
      :: MonadHttp m
      => Network.HTTP.Client.Request
      -> m Network.HTTP.Client.Request
    setQueryParams request =
      let queryString =
            renderSimpleQuery
              True
              [ ("info_hash", getHash torrentInfo.infoHash)
              , ("peer_id", BS8.pack $ T.unpack myPeerId)
              , ("port", "6881")
              , ("uploaded", "0")
              , ("downloaded", "0")
              , ("left", BS8.pack $ show torrentInfo.fileLength)
              , ("compact", "1")
              ]
      in  pure request {queryString}

doHandshake
  :: MonadIO m
  => FilePath
  -> PeerAddress
  -> m (PeerHandshake, Socket)
doHandshake filename (PeerAddress {host, port}) = liftIO $ do
  (socket, _) <- NS.connectSock (IPv4.encodeString host) (show port)
  sendHandshakeMessage socket =<< getTorrentInfo filename
  (,socket) <$> recv @PeerHandshake socket
  where
    sendHandshakeMessage :: Socket -> TorrentInfo -> IO ()
    sendHandshakeMessage socket torrentInfo =
      send
        socket
        PeerHandshake
          { infoHash = torrentInfo.infoHash
          , peerId = BS.replicate 20 0 -- can be anything
          }

-- | Download a piece.
--
-- This works by breaking the piece into 16 KiB blocks and requesting all
-- blocks.
downloadPiece :: MonadIO m => Socket -> FilePath -> TorrentInfo -> Int -> m _
downloadPiece socket outputFilename torrentInfo pieceIndex = liftIO $ do
  bitField <- recv @BitField socket
  fmtLn $ "" +|| bitField ||+ ""
  send socket Interested
  unchoke <- recv @Unchoke socket
  fmtLn $ "" +|| unchoke ||+ ""
  sendBlockRequest
  -- TODO: Receive all blocks.
  piece <- recv @Piece socket
  fmtLn $ "" +|| piece ||+ ""
  where
    sendBlockRequest = do
      send socket Request {index = 0, begin = 0, len = 16384}
