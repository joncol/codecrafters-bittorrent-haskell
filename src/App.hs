module App
  ( runCommand
  ) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Fmt
import Safe (headErr)
import System.IO

import AppEnv
import AppError
import AppMonad
import Bencode.Parser
import Messages.PeerHandshake
import Network
import Options
import Torrent.Info
import Torrent.MagnetLink
import Torrent.PeerAddress
import Util

runCommand :: Command -> AppM AppEnv IO ()
runCommand (DecodeCommand encodedValue) = do
  let decodedValue =
        fromRight (error "parse error") $
          parseOnly parseBencodeValue (BSE.encode BSE.latin1 encodedValue)
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
  (_, (mHandshakeResp, _)) <- doHandshake torrentInfo peerAddress
  case mHandshakeResp of
    Right handshakeResp ->
      liftIO . fmtLn $
        "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
    Left _ -> throwError InvalidHandshakeResponse
runCommand (DownloadPieceCommand outputFilename torrentFilename pieceIndex) = do
  torrentInfo <- getTorrentInfo torrentFilename
  peer <- getPeer torrentInfo
  pieceData <- do
    (socket, (_, leftovers)) <- doHandshake torrentInfo peer
    liftIO $ sendInterested socket leftovers
    downloadPiece socket torrentInfo pieceIndex
  liftIO . withFile outputFilename WriteMode $
    \hOut -> BS.hPut hOut pieceData
runCommand (DownloadCommand outputFilename torrentFilename) = do
  torrentInfo <- getTorrentInfo torrentFilename
  myPeerId <- asks myPeerId
  peers <- getPeers myPeerId torrentInfo
  sockets <- forM peers $ \peer -> do
    (socket, (_, leftovers)) <- doHandshake torrentInfo peer
    liftIO $ sendInterested socket leftovers
    pure socket
  download sockets outputFilename torrentInfo
runCommand (MagnetParseCommand magnetLinkStr) = do
  case parseOnly parseMagnetLink $ BSE.encode BSE.latin1 magnetLinkStr of
    Right magnetLink -> liftIO $ do
      fmtLn $ "Tracker URL: " +| fromMaybe "" magnetLink.mTrackerUrl |+ ""
      fmtLn $ "Info Hash: " +|| magnetLink.infoHash ||+ ""
    Left err -> error $ "parser error: " <> err

printTorrentInfo :: TorrentInfo -> IO ()
printTorrentInfo torrentInfo = do
  let pieceHashes :: [String] = map show torrentInfo.pieceHashes
  fmtLn $ "Tracker URL: " +| torrentInfo.trackerUrl |+ ""
  fmtLn $ "Length: " +| torrentInfo.fileLength |+ ""
  fmtLn $ "Info Hash: " +|| torrentInfo.infoHash ||+ ""
  fmtLn $ "Piece Length: " +| torrentInfo.pieceLength |+ ""
  fmtLn "Piece Hashes:"
  fmtLn $ unlinesF pieceHashes

getPeer :: TorrentInfo -> AppM AppEnv IO PeerAddress
getPeer torrentInfo = do
  myPeerId <- asks myPeerId
  peers <- getPeers myPeerId torrentInfo
  when (null peers) $ throwError NoPeersInTorrentFile
  pure $ headErr peers
