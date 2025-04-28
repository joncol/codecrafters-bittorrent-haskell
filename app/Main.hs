import Control.Monad (forM_)
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Binary qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BSB16
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.List.Split (chunksOf)
import Fmt
import Options.Applicative (execParser)
import System.IO

import Bencode.Parser
import Bencode.Types
import Bencode.Util
import Options

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts <- execParser options

  case opts.command of
    Decode encodedValue -> do
      let decodedValue =
            fromRight (error "parse error") $
              parseOnly parseBencodeValue (BSE.encode BSE.latin1 encodedValue)
          jsonValue = encode decodedValue
      LBS.putStr jsonValue
      putStr "\n"
    Info filename -> do
      contents <- BS.readFile filename
      let decodedValue =
            fromRight (error "parse error") $
              parseOnly parseBencodeValue contents
          keyVals = case decodedValue of
            BDict keyVals' -> keyVals'
            _ -> error "torrent file is not a Bencoded dictionary"
          trackerUrl = case getDictValue "announce" keyVals of
            Just (BString url) -> BSE.decode BSE.latin1 url
            _ -> error "no tracker URL in torrent file"
          infoKeyVals = case getDictValue "info" keyVals of
            Just (BDict keyVals') -> keyVals'
            _ -> error "no info dictionary in torrent file"
          len = case getDictValue "length" infoKeyVals of
            Just (BInt len') -> len'
            _ -> error "no length field in info dictionary"
          infoDictBS = LBS.toStrict . Bin.encode $ BDict infoKeyVals
          infoHash = SHA1.hash infoDictBS
          infoHashBase16 = BSE.decode BSE.latin1 $ BSB16.encode infoHash
          pieceLength = case getDictValue "piece length" infoKeyVals of
            Just (BInt len') -> len'
            _ -> error "no piece length field in info dictionary"
          pieceHashes = case getDictValue "pieces" infoKeyVals of
            Just (BString s) ->
              map (BSE.decode latin1 . BSB16.encode . BS.pack)
                . chunksOf 20
                $ BS.unpack s
            _ -> error "no piece length field in info dictionary"

      fmtLn $ "Tracker URL: " +| trackerUrl |+ ""
      fmtLn $ "Length: " +| len |+ ""
      fmtLn $ "Info Hash: " +| infoHashBase16 |+ ""
      fmtLn $ "Piece Length: " +| pieceLength |+ ""
      fmtLn "Piece Hashes:"
      forM_ pieceHashes $
        \p -> fmtLn $ build p
