module Messages.Extensions.Metadata.Data
  ( Data (..)
  , printData
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Fmt

import Bencode.Types
import Torrent.Hash
import Torrent.MagnetLink
import Util

data Data = Data
  { extensionMsgId :: Word8
  -- ^ This is the peer's metadata extension ID, which you received during the
  -- extension handshake.
  , pieceIndex :: Int64
  , totalSize :: Int64
  , pieceContents :: Bencode
  }
  deriving (Show)

msgId :: Word8
msgId = 20

msgType :: Integral a => a
msgType = 1

-- TODO: Support multi-piece metadata.
instance Binary Data where
  put Data {..} = do
    Bin.putWord32be $
      fromIntegral (dictSize + BSL.length (Bin.encode pieceContents) + 2)
    Bin.putWord8 msgId
    Bin.putWord8 extensionMsgId
    Bin.put dict
    Bin.put pieceContents
    where
      dict =
        BDict
          [ ("msg_type", BInt msgType)
          , ("piece", BInt pieceIndex)
          , ("total_size", BInt totalSize)
          ]
      dictSize = BSL.length $ Bin.encode dict
  get = do
    len <- Bin.getWord32be
    Bin.isolate (fromIntegral len) $ do
      msgId' <- Bin.getWord8
      guard $ msgId' == msgId
      extensionMsgId <- Bin.getWord8
      dict :: Bencode <- Bin.get
      pieceContents :: Bencode <- Bin.get
      case dict of
        BDict keyVals ->
          pure
            Data
              { pieceIndex = lookupJustBInt "piece" keyVals
              , totalSize = lookupJustBInt "total_size" keyVals
              , ..
              }
        _ -> error "expected a BDict in response to metadata data request"

printData :: MagnetLink -> Data -> IO ()
printData magnetLink d
  | BDict keyVals <- d.pieceContents = do
      fmtLn $ "Tracker URL: " +| fromMaybe "" magnetLink.mTrackerUrl |+ ""
      let pieceHashStrs :: [String] =
            map (show . Hash) . chunksOfBs 20 $ lookupJustBString "pieces" keyVals
      fmtLn $ "Length: " +| lookupJustBInt "length" keyVals |+ ""
      fmtLn $ "Info Hash: " +|| magnetLink.infoHash ||+ ""
      fmtLn $ "Piece Length: " +| lookupJustBInt "piece length" keyVals |+ ""
      fmtLn "Piece Hashes:"
      fmtLn $ unlinesF pieceHashStrs
  | otherwise = error "invalid `pieceData` type, expected a BDict"
