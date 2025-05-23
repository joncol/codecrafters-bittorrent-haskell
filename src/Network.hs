module Network
  ( send
  , recv
  , getPeers
  , doHandshake
  , doExtensionHandshake
  , recvExtensionHandshake
  , sendMetadataRequest
  , sendInterested
  , downloadPiece
  , download
  ) where

import Control.Concurrent.Async
import Control.Monad (forM_, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Lens.Family.State.Strict (zoom)
import Net.IPv4 qualified as IPv4
import Network.HTTP.Client (Request (..))
import Network.HTTP.Req hiding (port)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as NS
import Network.Socket.ByteString qualified as N
import Pipes.Binary qualified as P
import Pipes.Network.TCP qualified as P
import Pipes.Parse qualified as P
import System.IO
import Text.URI

import AppEnv
import AppError
import AppMonad
import Bencode.Types
import Bencode.Util
import Messages.BitField
import Messages.Extensions.Handshake
import Messages.Extensions.Metadata.Request qualified as Metadata
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

-- | Helper function that encodes and sends a message to a socket.
send :: Bin.Binary a => Socket -> a -> IO ()
send socket = NS.send socket . BSL.toStrict . Bin.encode

-- | Helper function that decodes a message received from a socket.
recv :: Bin.Binary a => Socket -> IO a
recv socket =
  Bin.decode . BSL.fromStrict <$> N.recv socket bufferSize

-- | Make a GET request to the torrent tracker to get a list of peers.
getPeers :: MonadIO m => Text -> TorrentInfo -> m [PeerAddress]
getPeers myPeerId torrentInfo = runReq defaultHttpConfig $ do
  uri <- mkURI torrentInfo.trackerUrl

  -- TODO: Is there a simpler way of handling the port?
  let port =
        (fromIntegral . fromMaybe 80)
          (either (const Nothing) (.authPort) uri.uriAuthority)

  case useURI uri of
    Just (Left (url, _)) -> doReq url port
    Just (Right (url, _)) -> doReq url port
    Nothing -> error "invalid tracker URL"
  where
    doReq :: MonadHttp m => Url scheme -> Int -> m [PeerAddress]
    doReq url port = do
      resp <- reqCb GET url NoReqBody bsResponse (Req.port port) setQueryParams

      case Bin.decode . BSL.fromStrict $ responseBody resp of
        BDict keyVals -> do
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
  => Hash
  -> PeerAddress
  -> AppM AppEnv m (Socket, (PeerHandshake, P.Producer BS.ByteString IO ()))
doHandshake infoHash (PeerAddress {host, port}) = do
  (socket, _) <- NS.connectSock (IPv4.encodeString host) (show port)
  liftIO $ sendHandshakeMessage socket
  (mResult, leftovers) <-
    liftIO
      . P.runStateT decodeHandshakeMessage
      $ P.fromSocket socket bufferSize
  case mResult of
    Left (P.DecodingError _ msg) -> throwError $ DecodingError (T.pack msg)
    Right peerHandshake -> do
      -- We must account for any leftovers from the handshake here.
      leftovers' <- liftIO . P.execStateT decodeBitField $ do
        leftovers
        P.fromSocket socket bufferSize
      pure (socket, (peerHandshake, leftovers'))
  where
    sendHandshakeMessage :: Socket -> IO ()
    sendHandshakeMessage socket = do
      peerId <- randomBytes 20
      send
        socket
        PeerHandshake
          { infoHash = infoHash
          , peerId
          , hasExtensionSupport = True
          }

    decodeHandshakeMessage
      :: P.Parser BS.ByteString IO (Either P.DecodingError PeerHandshake)
    decodeHandshakeMessage = P.decode

    decodeBitField
      :: P.Parser BS.ByteString IO (Either P.DecodingError BitField)
    decodeBitField = P.decode

doExtensionHandshake
  :: (MonadIO m, MonadError AppError m)
  => Socket
  -> P.Producer BS.ByteString IO ()
  -> m Word8
doExtensionHandshake socket leftovers = do
  let extensions = Map.fromList [("ut_metadata", 16)]
  liftIO $ send socket Handshake {extensions}
  extensionHandshakeResp <- recvExtensionHandshake socket leftovers
  pure . fromMaybe 0 $
    Map.lookup "ut_metadata" extensionHandshakeResp.extensions

-- | Note that the incoming extension handshake can happen before the outgoing
-- extension handshake.
recvExtensionHandshake
  :: forall m
   . (MonadIO m, MonadError AppError m)
  => Socket
  -> P.Producer BS.ByteString IO ()
  -> m Handshake
recvExtensionHandshake socket leftovers = do
  mResult <- liftIO . P.evalStateT decodeExtensionHandshakeMessage $ do
    leftovers
    P.fromSocket socket bufferSize
  case mResult of
    Left (P.DecodingError _ msg) -> throwError $ DecodingError (T.pack msg)
    Right extensionHandshake -> do
      pure extensionHandshake
  where
    decodeExtensionHandshakeMessage
      :: P.Parser BS.ByteString IO (Either P.DecodingError Handshake)
    decodeExtensionHandshakeMessage = P.decode

sendMetadataRequest :: forall m. MonadIO m => Socket -> Word8 -> m ()
sendMetadataRequest socket extensionMsgId = do
  liftIO $ send socket Metadata.Request {extensionMsgId, pieceIndex = 0}

sendInterested :: forall m. MonadIO m => Socket -> m ()
sendInterested socket = do
  liftIO $ send socket Interested
  void $ P.execStateT decodeUnchoke $ P.fromSocket socket bufferSize
  where
    decodeUnchoke :: P.Parser BS.ByteString m (Either P.DecodingError Unchoke)
    decodeUnchoke = P.decode

-- | Download a piece.
--
-- This works by breaking the piece into 16 KiB blocks and requesting all
-- blocks.
downloadPiece
  :: MonadIO m
  => Socket
  -> TorrentInfo
  -> Int
  -> m BS.ByteString
downloadPiece socket torrentInfo pieceIndex =
  liftIO $ do
    requestBlocks
    subPieces <- P.evalStateT decodeSubPieces $ P.fromSocket socket bufferSize
    pure $ foldMap block subPieces
  where
    blockLength = 16384

    pieceLen = getPieceLength torrentInfo pieceIndex

    requestBlocks = do
      let wholeBlockCount = pieceLen `div` blockLength
          lastBlockLength = pieceLen `mod` blockLength
          blockLengths =
            replicate wholeBlockCount blockLength
              <> [lastBlockLength | lastBlockLength > 0]

      forM_ ([0 ..] `zip` blockLengths) $ \(n, len) ->
        send
          socket
          Request
            { index = fromIntegral pieceIndex
            , begin = n * fromIntegral blockLength
            , len = fromIntegral len
            }

    decodeSubPieces :: P.Parser BS.ByteString IO [Piece]
    decodeSubPieces = zoom (P.decoded . P.splitAt blockCount) P.drawAll
      where
        blockCount =
          ceiling $
            (((/) :: Double -> Double -> Double) `on` fromIntegral)
              pieceLen
              blockLength

-- | Download a torrent.
download
  :: forall m
   . MonadIO m
  => [Socket]
  -> FilePath
  -> TorrentInfo
  -> AppM AppEnv m ()
download sockets outputFilename torrentInfo = do
  let pieceCount = length torrentInfo.pieceHashes
  pieces <- go [0 .. pieceCount] []
  liftIO $ do
    withFile outputFilename WriteMode . const $ pure () -- Truncate file.
    withFile outputFilename AppendMode $ \hOut ->
      BS.hPut hOut . mconcat . map snd $ sortOn fst pieces
  where
    go
      :: [Int]
      -> [(Int, BS.ByteString)]
      -> AppM AppEnv m [(Int, BS.ByteString)]
    go [] acc = pure acc
    go unfinishedPieces acc = do
      results <- liftIO . forConcurrently (unfinishedPieces `zip` sockets) $
        \(pieceIndex, socket) ->
          (pieceIndex,) <$> downloadPiece socket torrentInfo pieceIndex
      let rest = drop (length sockets) unfinishedPieces
      go rest $ acc <> results
