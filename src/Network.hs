module Network
  ( send
  , recv
  , getPeers
  , doHandshake
  , recvExtensionHandshake
  , sendMetadataRequest
  , sendInterested
  , recvUnchoke
  , downloadPiece
  , download
  ) where

import Control.Concurrent.Async
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (foldMap')
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
import Messages.Extensions.Handshake qualified as Extensions
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

-- | Does a peer handshake and waits for a "bitfield" message.
doHandshake
  :: MonadIO m
  => Hash
  -> PeerAddress
  -> AppM AppEnv m (Socket, (PeerHandshake, Maybe Word8))
doHandshake infoHash (PeerAddress {host, port}) = do
  -- liftIO . putStrLn $ "-> doHandshake, " <> IPv4.encodeString host <> ":" <> show port
  (socket, _) <- NS.connectSock (IPv4.encodeString host) (show port)
  liftIO $ sendHandshakeMessage socket
  (mPeerHandshake, leftovers) <-
    liftIO . P.runStateT decodeHandshakeMessage $
      P.fromSocket socket bufferSize
        -- >-> P.tee
        --   ( P.mapM_ $ \bs ->
        --       let t = BSE.decode BSE.latin1 (BSB16.encode bs)
        --       in  fmtLn $ "received data: " +| t |+ ""
        --   )

  -- liftIO $ putStrLn "handshake completed"

  -- Lift the 'Either' value into a 'MonadError AppError'.
  peerHandshake <- liftEither mPeerHandshake

  mMetadataId <-
    if peerHandshake.hasExtensionSupport
      then Just <$> doExtensionHandshake socket leftovers
      else pure Nothing

  pure (socket, (peerHandshake, mMetadataId))
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
      :: P.Parser BS.ByteString IO (Either AppError PeerHandshake)
    decodeHandshakeMessage = do
      runExceptT . withExceptT transformError $ do
        peerHandshake <- ExceptT P.decode
        _bitField :: BitField <- ExceptT P.decode -- TODO: Can we use 'void' instead?
        pure peerHandshake
      where
        transformError (P.DecodingError _ msg) = DecodingError (T.pack msg)

-- | Does an extension handshake and returns the ID used by the peer for the
-- metadata extension.
doExtensionHandshake
  :: (MonadIO m, MonadError AppError m)
  => Socket
  -> P.Producer BS.ByteString IO ()
  -> m Word8
doExtensionHandshake socket leftovers = do
  -- liftIO $ putStrLn "-> doExtensionHandshake"
  let extensions = Map.fromList [("ut_metadata", 16)]
  liftIO $ send socket Extensions.Handshake {extensions}
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
  -> m Extensions.Handshake
recvExtensionHandshake socket leftovers = do
  mResult <-
    liftIO . P.evalStateT decodeExtensionHandshakeMessage $ do
      leftovers
      P.fromSocket socket bufferSize
  case mResult of
    Left (P.DecodingError _ msg) -> throwError $ DecodingError (T.pack msg)
    Right extensionHandshake -> do
      pure extensionHandshake

decodeExtensionHandshakeMessage
  :: P.Parser BS.ByteString IO (Either P.DecodingError Extensions.Handshake)
decodeExtensionHandshakeMessage = P.decode

sendMetadataRequest :: forall m. MonadIO m => Socket -> Word8 -> m ()
sendMetadataRequest socket extensionMsgId = do
  liftIO $ send socket Metadata.Request {extensionMsgId, pieceIndex = 0}

sendInterested :: forall m. MonadIO m => Socket -> m ()
sendInterested socket = liftIO $ send socket Interested

recvUnchoke
  :: forall m
   . MonadIO m
  => Socket
  -> m ()
recvUnchoke socket = do
  -- liftIO $ putStrLn "-> recvUnchoke"
  _unchoke <- P.evalStateT decodeUnchoke $ do
    P.fromSocket socket bufferSize
      -- >-> P.tee
      --   ( P.mapM_ $ \bs ->
      --       let t = BSE.decode BSE.latin1 (BSB16.encode bs)
      --       in  liftIO . fmtLn $ "received data: " +| t |+ ""
      --   )
  -- liftIO . putStrLn $ "decoded unchoke " <> show unchoke <> " on socket: " <> show socket
  pure ()
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
    -- putStrLn $ "-> downloadPiece, socket: " <> show socket
    -- putStrLn $ "  pieceIndex: " <> show pieceIndex

    requestBlocks

    -- putStrLn "requested blocks"

    subPieces <-
      P.evalStateT decodeSubPieces $ do
        P.fromSocket socket bufferSize
          -- >-> P.tee
          --   ( P.mapM_ $ \bs ->
          --       let t = BSE.decode BSE.latin1 (BSB16.encode bs)
          --       in  fmtLn $ "received data: " +| t |+ ""
          --   )

    -- >-> P.tee
    --   ( P.mapM_ $ \bs ->
    --       let t = BSE.decode BSE.latin1 (BSB16.encode bs)
    --       in  fmtLn $ "received data: " +| t |+ ""
    --   )

    -- subPiece <- P.evalStateT decodeSubPiece $ do
    --   P.fromSocket socket bufferSize
    --     >-> P.tee
    --       ( P.mapM_ $ \bs ->
    --           let t = BSE.decode BSE.latin1 (BSB16.encode bs)
    --           in  fmtLn $ "received data: " +| t |+ ""
    --       )

    -- case subPiece of
    --   Right sp -> do
    --     fmtLn $ "sub piece received: "
    --     fmtLn $ "  msgLen: " +| hexF sp.msgLen |+ ""
    --     fmtLn $ "  msgId: " +| hexF sp.mid |+ ""
    --     fmtLn $ "  index: " +| hexF sp.index |+ ""
    --     fmtLn $ "  begin: " +| hexF sp.begin |+ ""
    --   Left e -> do
    --     putStrLn $ "decoding error: " <> show e
    --     error "BREAK NOW"
    -- pure $ foldMap' block []

    -- forM_ subPieces $ \sp -> do
    --   fmtLn "sub piece received: "
    --   fmtLn $ "  msgLen: " +| hexF sp.msgLen |+ ""
    --   fmtLn $ "  msgId: " +| hexF sp.mid |+ ""
    --   fmtLn $ "  index: " +| hexF sp.index |+ ""
    --   fmtLn $ "  begin: " +| hexF sp.begin |+ ""

    pure $ foldMap' block subPieces
  where
    blockLength = 16384

    pieceLen = getPieceLength torrentInfo pieceIndex

    requestBlocks = do
      let wholeBlockCount = pieceLen `div` blockLength
          lastBlockLength = pieceLen `mod` blockLength
          blockLengths =
            replicate wholeBlockCount blockLength
              <> [lastBlockLength | lastBlockLength > 0]

      -- putStrLn $ "pieceLen: " <> show pieceLen
      -- putStrLn $ "wholeBlockCount: " <> show wholeBlockCount
      -- putStrLn $ "lastBlockLength: " <> show lastBlockLength
      -- putStrLn $ "blockLength: " <> show blockLength
      -- putStrLn $ "blockLengths: " <> show blockLengths

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
  pieces <- go [0 .. pieceCount - 1] []
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
      -- results <- liftIO . forM (unfinishedPieces `zip` sockets) $
      --   \(pieceIndex, socket) ->
      --     (pieceIndex,) <$> downloadPiece socket torrentInfo pieceIndex
      results <- liftIO . forConcurrently (unfinishedPieces `zip` sockets) $
        \(pieceIndex, socket) ->
          (pieceIndex,) <$> downloadPiece socket torrentInfo pieceIndex
      let rest = drop (length sockets) unfinishedPieces
      go rest $ acc <> results
