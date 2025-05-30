module App
  ( runCommand
  ) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Fmt
import Network.Socket (close)
import Safe (headErr)
import System.IO

import AppEnv
import AppError
import AppMonad
import Bencode.Types
import Messages.Extensions.Metadata.Data qualified as Metadata
import Messages.PeerHandshake
import Network
import Options
import Torrent.Info
import Torrent.MagnetLink
import Torrent.PeerAddress
import Util

runCommand :: Command -> AppM AppEnv IO ()
runCommand (DecodeCommand encodedValue) = do
  let decodedValue :: Bencode =
        Bin.decode $ BSL.fromStrict (BSE.encode BSE.latin1 encodedValue)
      jsonValue = Aeson.encode decodedValue
  liftIO $ do
    BSL.putStr jsonValue
    putStr "\n"
runCommand (InfoCommand filename) = do
  torrentInfo <- getTorrentInfo filename
  liftIO $ printTorrentInfo torrentInfo
runCommand (PeersCommand filename) = do
  myPeerId <- asks myPeerId

  getTorrentInfo filename
    >>= getPeers myPeerId
    >>= \peers -> liftIO . fmtLn $ unlinesF (map show peers)
runCommand (HandshakeCommand filename peerAddress) = do
  torrentInfo <- getTorrentInfo filename
  (_, (handshakeResp, _)) <- doHandshake torrentInfo.infoHash peerAddress False
  liftIO . fmtLn $
    "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
runCommand (DownloadPieceCommand outputFilename torrentFilename pieceIndex) = do
  torrentInfo <- getTorrentInfo torrentFilename
  peerAddress <- getPeerAddress torrentInfo
  pieceData <- do
    (socket, _) <- doHandshake torrentInfo.infoHash peerAddress True
    sendInterested socket
    recvUnchoke socket
    downloadPiece socket torrentInfo pieceIndex
  liftIO . withFile outputFilename WriteMode $
    \hOut -> BS.hPut hOut pieceData
runCommand (DownloadCommand outputFilename torrentFilename) = do
  torrentInfo <- getTorrentInfo torrentFilename
  myPeerId <- asks myPeerId
  peers <- getPeers myPeerId torrentInfo
  sockets <- forM peers $ \peer -> do
    (socket, _) <- doHandshake torrentInfo.infoHash peer True
    sendInterested socket
    recvUnchoke socket
    pure socket
  download sockets outputFilename torrentInfo
runCommand (MagnetParseCommand magnetLinkStr) = do
  case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
    Right magnetLink -> liftIO $ do
      fmtLn $ "Tracker URL: " +| fromMaybe "" magnetLink.mTrackerUrl |+ ""
      fmtLn $ "Info Hash: " +|| magnetLink.infoHash ||+ ""
    Left err -> error $ "parser error: " <> err
runCommand (MagnetHandshakeCommand magnetLinkStr) = do
  case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
    Right magnetLink -> do
      peerAddress <- getPeerAddress $ magnetLinkToTorrentInfo magnetLink
      (_, (handshakeResp, mMetadataId)) <-
        doHandshake magnetLink.infoHash peerAddress True

      liftIO
        . fmtLn
        $ "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""

      whenJust mMetadataId $ \metadataId ->
        liftIO . fmtLn $ "Peer Metadata Extension ID: " +| metadataId |+ ""
    Left err -> error $ "parser error: " <> err
runCommand (MagnetInfoCommand magnetLinkStr) = do
  case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
    Right magnetLink -> do
      peerAddress <- getPeerAddress $ magnetLinkToTorrentInfo magnetLink
      (socket, (_, mMetadataId)) <-
        doHandshake magnetLink.infoHash peerAddress True
      whenJust mMetadataId $ \metadataId -> do
        sendMetadataRequest socket metadataId
        liftIO $ do
          data' :: Metadata.Data <- recv socket
          Metadata.printData magnetLink data'
    Left err -> error $ "parser error: " <> err
runCommand
  (MagnetDownloadPieceCommand outputFilename magnetLinkStr pieceIndex) = do
    case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
      Right magnetLink -> do
        -- First do base handshake with the first peer.
        let torrentInfo = magnetLinkToTorrentInfo magnetLink
        peerAddress <- getPeerAddress torrentInfo
        (socket, (_, mMetadataId)) <-
          doHandshake magnetLink.infoHash peerAddress True

        -- When the peer has extension support, we do an extension handshake
        -- with it, to be able to get a hold of the needed torrent metadata.
        whenJust mMetadataId $ \metadataId -> do
          sendMetadataRequest socket metadataId
          data' :: Metadata.Data <- liftIO $ recv socket
          let torrentInfo' =
                case data'.pieceContents of
                  BDict keyVals ->
                    let pieceLength =
                          fromIntegral $ lookupJustBInt "piece length" keyVals
                        fileLength = lookupJustBInt "length" keyVals
                        pieceHashes =
                          map Hash . chunksOfBs 20 $
                            lookupJustBString "pieces" keyVals
                    in  torrentInfo {fileLength, pieceLength, pieceHashes}
                  _ -> error "invalid metadata"

          sendInterested socket
          recvUnchoke socket

          pieceData <- downloadPiece socket torrentInfo' pieceIndex
          liftIO . withFile outputFilename WriteMode $
            \hOut -> BS.hPut hOut pieceData
      Left err -> error $ "parser error: " <> err
runCommand (MagnetDownloadCommand outputFilename magnetLinkStr) = do
  case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
    Right magnetLink -> do
      -- First do base handshake with the first peer.
      let torrentInfo = magnetLinkToTorrentInfo magnetLink
      peerAddress <- getPeerAddress torrentInfo
      (socket, (_, mMetadataId)) <-
        doHandshake magnetLink.infoHash peerAddress True

      -- When the peer has extension support, we do an extension handshake with
      -- it, to be able to get a hold of the needed torrent metadata.
      whenJust mMetadataId $ \metadataId -> do
        sendMetadataRequest socket metadataId
        data' :: Metadata.Data <- liftIO $ recv socket
        let torrentInfo' =
              case data'.pieceContents of
                BDict keyVals ->
                  let pieceLength =
                        fromIntegral $ lookupJustBInt "piece length" keyVals
                      fileLength = lookupJustBInt "length" keyVals
                      pieceHashes =
                        map Hash . chunksOfBs 20 $
                          lookupJustBString "pieces" keyVals
                  in  torrentInfo {fileLength, pieceLength, pieceHashes}
                _ -> error "invalid metadata"
        liftIO $ close socket
        myPeerId <- asks myPeerId
        peers <- getPeers myPeerId torrentInfo'
        sockets <- forM peers $ \peer -> do
          (socket', _) <- doHandshake torrentInfo'.infoHash peer True
          sendInterested socket'
          recvUnchoke socket'
          pure socket'
        download sockets outputFilename torrentInfo'
    Left err -> error $ "parser error: " <> err

magnetLinkToTorrentInfo :: MagnetLink -> TorrentInfo
magnetLinkToTorrentInfo magnetLink =
  TorrentInfo
    { trackerUrl = fromMaybe "" magnetLink.mTrackerUrl
    , fileLength = 999 -- Just use any value that's greater than 0.
    , infoHash = magnetLink.infoHash
    , pieceLength = 0
    , pieceHashes = []
    }

getPeerAddress :: TorrentInfo -> AppM AppEnv IO PeerAddress
getPeerAddress torrentInfo = do
  myPeerId <- asks myPeerId
  peers <- getPeers myPeerId torrentInfo
  when (null peers) $ throwError NoPeersInTorrentFile
  pure $ headErr peers
