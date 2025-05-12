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
import Messages.PeerHandshake
import Torrent.Hash
import Torrent.Info
import Torrent.PeerAddress
import Util

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
  (,socket) <$> recvHandshake socket
  where
    sendHandshakeMessage :: Socket -> TorrentInfo -> IO ()
    sendHandshakeMessage socket torrentInfo =
      NS.send socket . BSL.toStrict . Bin.encode $
        PeerHandshake
          { infoHash = torrentInfo.infoHash
          , peerId = BS.replicate 20 0 -- can be anything
          }

    recvHandshake :: Socket -> IO PeerHandshake
    recvHandshake socket =
      Bin.decode . BSL.fromStrict <$> N.recv socket 4096

downloadPiece :: MonadIO m => Socket -> FilePath -> Int -> m _
downloadPiece socket outputFilename pieceIndex = _
