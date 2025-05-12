module App (
  runCommand
) where

import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as BS
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Text (Text)
import Fmt
import Options.Applicative (execParser)
import System.IO

import Bencode.Parser
import Messages.GetPeers
import Messages.PeerHandshake
import Options
import Torrent.Info
import Util

runCommand :: Text -> Command -> IO ()
runCommand _myPeerId (DecodeCommand encodedValue) = do
  let decodedValue =
        fromRight (error "parse error") $
          parseOnly parseBencodeValue (BSE.encode BSE.latin1 encodedValue)
      jsonValue = Aeson.encode decodedValue
  BSL.putStr jsonValue
  putStr "\n"
runCommand _myPeerId (InfoCommand filename) = do
  torrentInfo <- getTorrentInfo filename

  let pieceHashes :: [String] = map show torrentInfo.pieceHashes

  fmtLn $ "Tracker URL: " +| torrentInfo.trackerUrl |+ ""
  fmtLn $ "Length: " +| torrentInfo.fileLength |+ ""
  fmtLn $ "Info Hash: " +|| torrentInfo.infoHash ||+ ""
  fmtLn $ "Piece Length: " +| torrentInfo.pieceLength |+ ""
  fmtLn "Piece Hashes:"
  fmtLn $ unlinesF pieceHashes
runCommand myPeerId (PeersCommand filename) =
  getTorrentInfo filename
    >>= getPeers myPeerId
    >>= \peers -> fmtLn $ unlinesF (map show peers)
runCommand _myPeerId (HandshakeCommand filename peerAddress) = do
  handshakeResp <- doHandshake filename peerAddress
  fmtLn $
    "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
runCommand
  myPeerId
  (DownloadPieceCommand outputFilename torrentFilename pieceIndex) = do
    peers <- getPeers myPeerId =<< getTorrentInfo torrentFilename
    case peers of
      peer : _ -> do
        handshakeResp <- doHandshake torrentFilename peer
        fmtLn $
          "Peer ID: " +| foldMap byteF (BS.unpack handshakeResp.peerId) |+ ""
      [] -> error "no peers found in torrent file"
    fmtLn "TODO: implement download_piece"
