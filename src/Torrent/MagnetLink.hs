module Torrent.MagnetLink
  ( MagnetLink (..)
  , parseMagnetLink
  ) where

import Control.Monad (void)
import Data.Array.Byte (ByteArray)
import Data.Attoparsec.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Encoding qualified as BSE
import Data.Bytes.Parser qualified as BytesParser
import Data.Bytes.Parser.Latin qualified as BytesParser
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Primitive.Array (Array)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.IsList (IsList (fromList))
import Network.HTTP.Types.URI (parseSimpleQuery)

import Torrent.Hash

data MagnetLink = MagnetLink
  { infoHash :: Hash
  , mFilename :: Maybe FilePath
  , mTrackerUrl :: Maybe Text
  }
  deriving (Show)

parseMagnetLink :: Parser MagnetLink
parseMagnetLink = do
  void $ string "magnet:"
  queryStr <- takeByteString
  let query :: [(BS.ByteString, BS.ByteString)] = parseSimpleQuery queryStr
  let infoHashStr = fromMaybe "" $ lookup "xt" query
  let infoHashStr' = fromJust $ BS.stripPrefix "urn:btih:" infoHashStr
  let byteArray :: ByteArray = fromList $ BS.unpack infoHashStr'
  let err = "error parsing hash" :: String
  let res = BytesParser.parseByteArray (parseHash err) byteArray
  -- TODO: Make this error handling nicer.
  case res of
    BytesParser.Failure e -> error e
    BytesParser.Success (BytesParser.Slice _ _ infoHash) -> do
      let mFilename = BS8.unpack <$> lookup "dn" query
      let mTrackerUrl = BSE.decode BSE.latin1 <$> lookup "tr" query
      pure MagnetLink {..}

parseHash :: e -> BytesParser.Parser e s Hash
parseHash err = do
  bytes :: Array Word8 <-
    BytesParser.replicate 20 $ BytesParser.hexFixedWord8 err

  pure . Hash . BS.pack $ toList bytes
