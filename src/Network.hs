module Network
  ( getPeers
  , doHandshake
  , downloadPiece
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString (parseOnly)
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Fmt
import Lens.Family.State.Strict (zoom)
import Net.IPv4 qualified as IPv4
import Network.HTTP.Client (Request (..))
import Network.HTTP.Req hiding (port)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as NS
import Network.Socket.ByteString qualified as N
import Pipes ((>->))
import Pipes qualified as P
import Pipes.Binary qualified as P
import Pipes.ByteString qualified as PBS
import Pipes.Network.TCP qualified as P
import Pipes.Parse qualified as P
import Pipes.Prelude qualified as P
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
import System.IO
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
  fmtLn $ "recv bs: " +| foldMap byteF (BS.unpack bs) |+ ""
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
          (either (const Nothing) (.authPort) uri.uriAuthority)

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
  -> m
      ( Socket
      , (Either P.DecodingError PeerHandshake, P.Producer BS.ByteString IO ())
      )
doHandshake filename (PeerAddress {host, port}) = liftIO $ do
  (socket, _) <- NS.connectSock (IPv4.encodeString host) (show port)
  sendHandshakeMessage socket =<< getTorrentInfo filename
  -- (\peerHandshake -> (socket, peerHandshake, leftovers)) <$> recv @PeerHandshake socket
  (socket,)
    <$> P.runStateT decodeHandshakeMessage (P.fromSocket socket bufferSize)
  where
    sendHandshakeMessage :: Socket -> TorrentInfo -> IO ()
    sendHandshakeMessage socket torrentInfo = do
      peerId <- randomBytes 20
      send
        socket
        PeerHandshake
          { infoHash = torrentInfo.infoHash
          , peerId
          }

    -- decodeHandshakeMessage :: P.Parser BS.ByteString IO (Maybe PeerHandshake)
    -- decodeHandshakeMessage = zoom P.decoded P.draw

    decodeHandshakeMessage
      :: P.Parser BS.ByteString IO (Either P.DecodingError PeerHandshake)
    decodeHandshakeMessage = P.decode

-- | Download a piece.
--
-- This works by breaking the piece into 16 KiB blocks and requesting all
-- blocks.
downloadPiece
  :: MonadIO m
  => Socket
  -> P.Producer BS.ByteString IO ()
  -> FilePath
  -> TorrentInfo
  -> Int
  -> m ()
downloadPiece socket leftovers outputFilename torrentInfo pieceIndex = liftIO $ do
  -- void $ recv @BitField socket
  -- send socket Interested
  -- void $ recv @Unchoke socket
  -- requestBlocks

  fmtLn "-> downloadPiece"

  -- We must account for leftovers from earlier handshake here.
  leftovers' <- P.execStateT decodeBitField $ do
    leftovers
    P.fromSocket socket bufferSize

  -- bf <- recv @BitField socket
  -- fmtLn $ "bitfield: " +|| bf ||+ ""
  send socket Interested
  -- uc <- recv @Unchoke socket
  -- fmtLn $ "unchoke: " +|| uc ||+ ""
  leftovers'' <- P.execStateT decodeUnchoke $ do
    leftovers'
    P.fromSocket socket bufferSize

  requestBlocks

  pieces <- P.evalStateT decodePieces $ do
    leftovers''
    P.fromSocket socket bufferSize
  fmtLn $ "Pieces: " +|| map (\p -> (p.index, p.begin)) pieces ||+ ""

  withFile outputFilename WriteMode $ \hOut ->
    P.runEffect $ P.each pieces >-> P.map block >-> PBS.toHandle hOut
  where
    decodeBitField :: P.Parser BS.ByteString IO ()
    decodeBitField = do
      Right bf :: Either P.DecodingError BitField <- P.decode
      liftIO . fmtLn $ "bitfield: " +|| bf ||+ ""
      pure ()

    decodeUnchoke :: P.Parser BS.ByteString IO ()
    decodeUnchoke = do
      Right uc :: Either P.DecodingError Unchoke <- P.decode
      liftIO . fmtLn $ "unchoke: " +|| uc ||+ ""
      pure ()

    blockLength = 16384

    pieceLen = getPieceLength torrentInfo pieceIndex

    requestBlocks = do
      let wholeBlockCount = pieceLen `div` blockLength
          lastBlockLength = pieceLen `mod` blockLength
          blockLengths =
            replicate wholeBlockCount blockLength
              <> [lastBlockLength | lastBlockLength > 0]

      putStrLn $ "piece index: " <> show pieceIndex
      putStrLn $ "piece length: " <> show torrentInfo.pieceLength
      putStrLn $ "this piecelen: " <> show pieceLen
      putStrLn $ "requested block lengths: " <> show blockLengths

      forM_ ([0 ..] `zip` blockLengths) $ \(n, len) ->
        send
          socket
          Request
            { index = fromIntegral pieceIndex
            , begin = n * fromIntegral blockLength
            , len = fromIntegral len
            }

    decodePieces :: P.Parser BS.ByteString IO [Piece]
    decodePieces = zoom (P.decoded . P.splitAt blockCount) P.drawAll
      where
        blockCount =
          ceiling $
            (((/) :: Double -> Double -> Double) `on` fromIntegral)
              pieceLen
              blockLength
