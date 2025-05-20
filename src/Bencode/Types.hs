module Bencode.Types
  ( Bencode (..)
  ) where

import Control.Applicative (asum, many)
import Control.Monad (forM_, guard, void)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Bifunctor (bimap)
import Data.Binary (Binary, Get)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Get.Internal qualified as BinInt
import Data.Binary.Put qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word8

data Bencode
  = BString BS.ByteString
  | BInt Int64
  | BList [Bencode]
  | BDict [(Text, Bencode)]
  deriving (Show)

instance A.ToJSON Bencode where
  toJSON (BString bs) = A.String $ BSE.decode BSE.latin1 bs
  toJSON (BInt n) = A.Number $ fromIntegral n
  toJSON (BList xs) = A.Array . V.fromList $ map A.toJSON xs
  toJSON (BDict keyVals) =
    A.Object . A.fromList $ map (bimap A.fromText A.toJSON) keyVals

instance Binary Bencode where
  put (BString bs) = do
    Bin.putByteString . BS8.pack . show $ BS.length bs
    Bin.put ':'
    Bin.putByteString bs
  put (BInt n) = do
    Bin.put 'i'
    Bin.putByteString . BS8.pack $ show n
    Bin.put 'e'
  put (BList xs) = do
    Bin.put 'l'
    traverse_ Bin.put xs
    Bin.put 'e'
  put (BDict keyVals) = do
    let keyVals' = sortOn fst keyVals
    Bin.put 'd'
    forM_ keyVals' $ \(key, value) -> do
      Bin.put . BString $ BSE.encode BSE.latin1 key
      Bin.put value
    Bin.put 'e'

  get = asum [getBString, getBInt, getBList, getBDict]

getBString :: Get Bencode
getBString = do
  len <- getInt
  c <- Bin.getWord8
  guard $ c == _colon
  bs <- Bin.getByteString len
  pure $ BString bs

getBInt :: Get Bencode
getBInt = do
  i <- Bin.getWord8
  guard $ i == _i
  n <- getInt
  e <- Bin.getWord8
  guard $ e == _e
  pure $ BInt n

getBList :: Get Bencode
getBList = do
  l <- Bin.getWord8
  guard $ l == _l
  items :: [Bencode] <- many Bin.get
  e <- Bin.getWord8
  guard $ e == _e
  pure $ BList items

getBDict :: Get Bencode
getBDict = do
  d <- Bin.getWord8
  guard $ d == _d
  keyVals :: [(Text, Bencode)] <- many getKeyVal
  e <- Bin.getWord8
  guard $ e == _e
  pure $ BDict keyVals

-- | Helper function to get an int encoded as decimal ASCII.
getInt :: Integral a => Get a
getInt = do
  void $ Bin.lookAhead getAsciiDigit
  go 0
  where
    getAsciiDigit = do
      c <- Bin.getWord8
      if _0 <= c && c <= _9
        then pure c
        else fail "not an ASCII digit"

    go acc = do
      c <- peek
      if _0 <= c && c <= _9
        then do
          void Bin.getWord8
          let n = fromIntegral $ c - _0
          go $ acc * 10 + n
        else do
          pure acc

getKeyVal :: Get (Text, Bencode)
getKeyVal = do
  BString key <- getBString
  val <- Bin.get
  pure (BSE.decode BSE.latin1 key, val)

-- | Match any byte, to perform lookahead. Does not consume any input, but will
-- fail if end of input has been reached.
peek :: Get Word8
peek = do
  BinInt.ensureN 1
  BSU.unsafeHead <$> BinInt.get
{-# INLINE peek #-}
