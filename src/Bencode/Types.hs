module Bencode.Types
  ( Bencode (..)
  ) where

import Control.Monad (forM_)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Bifunctor (bimap)
import Data.Binary (Binary, get, put)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Encoding qualified as BSE
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Vector qualified as V

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
    putByteString . BS8.pack . show $ BS.length bs
    put ':'
    putByteString bs
  put (BInt n) = do
    put 'i'
    putByteString . BS8.pack $ show n
    put 'e'
  put (BList xs) = do
    put 'l'
    traverse_ put xs
    put 'e'
  put (BDict keyVals) = do
    let keyVals' = sortOn fst keyVals
    put 'd'
    forM_ keyVals' $ \(key, value) -> do
      put . BString $ BSE.encode BSE.latin1 key
      put value
    put 'e'
  get = error "not implemented"
