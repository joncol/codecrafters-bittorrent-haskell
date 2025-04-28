import Control.Monad (forM_)
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Base16 qualified as BSB16
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Fmt
import Options.Applicative (execParser)
import System.IO

import Bencode.Parser
import Options
import Torrent.Info

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
      LBS.putStr jsonValue
      putStr "\n"
    InfoCommand filename -> do
      torrentInfo <- getTorrentInfo filename
      let infoHashBase16 =
            BSE.decode BSE.latin1 $ BSB16.encode torrentInfo.infoHash
          pieceHashesBase16 =
            map (BSE.decode latin1 . BSB16.encode) torrentInfo.pieceHashes

      fmtLn $ "Tracker URL: " +| torrentInfo.trackerUrl |+ ""
      fmtLn $ "Length: " +| torrentInfo.length |+ ""
      fmtLn $ "Info Hash: " +| infoHashBase16 |+ ""
      fmtLn $ "Piece Length: " +| torrentInfo.pieceLength |+ ""
      fmtLn "Piece Hashes:"
      forM_ pieceHashesBase16 $ \p -> fmtLn $ build p
