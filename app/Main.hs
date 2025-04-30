import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Fmt
import Network.HTTP.Client (Request (..))
import Network.HTTP.Req hiding (port)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.URI (renderSimpleQuery)
import Options.Applicative (execParser)
import System.IO
import Text.URI

import Bencode.Parser
import Bencode.Types
import Bencode.Util
import Options
import Torrent.Info
import Torrent.Peer hiding (port)
import Util

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts <- execParser options

  case opts.command of
    DecodeCommand encodedValue -> do
      let decodedValue =
            fromRight (error "parse error") $
              parseOnly parseBencodeValue (BSE.encode BSE.latin1 encodedValue)
          jsonValue = encode decodedValue
      BSL.putStr jsonValue
      putStr "\n"
    InfoCommand filename -> do
      torrentInfo <- getTorrentInfo filename

      fmtLn $ "Tracker URL: " +| torrentInfo.trackerUrl |+ ""
      fmtLn $ "Length: " +| torrentInfo.length |+ ""
      fmtLn $ "Info Hash: " +|| torrentInfo.infoHash ||+ ""
      fmtLn $ "Piece Length: " +| torrentInfo.pieceLength |+ ""
      fmtLn "Piece Hashes:"
      forM_ torrentInfo.pieceHashes $ \peer -> fmtLn $ "" +|| peer ||+ ""
    PeersCommand filename ->
      getTorrentInfo filename
        >>= getPeers
        >>= traverse_ (\peer -> fmtLn $ "" +|| peer ||+ "")

-- | Make a GET request to the torrent tracker to get a list of peers.
getPeers :: MonadIO m => TorrentInfo -> m [Peer]
getPeers torrentInfo = runReq defaultHttpConfig $ do
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
    doReq :: MonadHttp m => Url scheme -> Int -> m [Peer]
    doReq url port = do
      resp <- reqCb GET url NoReqBody bsResponse (Req.port port) setQueryParams

      case parseOnly parseBencodeValue $ responseBody resp of
        Right (BDict keyVals) -> do
          let peersBStr = case getDictValue "peers" keyVals of
                Just (BString str) -> str
                _ -> error "tracker response did not contain `peers` field"
          pure . map bsToPeer $ chunksOfBs 6 peersBStr
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
              , ("peer_id", "my_peer_id_hello_abc")
              , ("port", "6881")
              , ("uploaded", "0")
              , ("downloaded", "0")
              , ("left", BS8.pack $ show torrentInfo.length)
              , ("compact", "1")
              ]
      in  pure request {queryString}
