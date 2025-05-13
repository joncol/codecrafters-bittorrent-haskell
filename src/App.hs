module App
  ( runCommand
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Fmt
import Safe (headErr)

import AppEnv
import AppError
import AppMonad
import Bencode.Parser
import Messages.PeerHandshake
import Network
import Options
import Torrent.Info
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
  (handshakeResp, _) <- doHandshake filename peerAddress
  liftIO . fmtLn $
    "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
runCommand
  (DownloadPieceCommand outputFilename torrentFilename pieceIndex) = do
    myPeerId <- asks myPeerId
    torrentInfo <- getTorrentInfo torrentFilename
    peers <- getPeers myPeerId torrentInfo
    when (null peers) $ throwError NoPeersInTorrentFile
    let peer = headErr peers
    (_, socket) <- doHandshake torrentFilename peer
    downloadPiece socket outputFilename torrentInfo pieceIndex

printTorrentInfo :: TorrentInfo -> IO ()
printTorrentInfo torrentInfo = do
  let pieceHashes :: [String] = map show torrentInfo.pieceHashes
  fmtLn $ "Tracker URL: " +| torrentInfo.trackerUrl |+ ""
  fmtLn $ "Length: " +| torrentInfo.fileLength |+ ""
  fmtLn $ "Info Hash: " +|| torrentInfo.infoHash ||+ ""
  fmtLn $ "Piece Length: " +| torrentInfo.pieceLength |+ ""
  fmtLn "Piece Hashes:"
  fmtLn $ unlinesF pieceHashes
