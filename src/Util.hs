module Util
  ( chunksOfBs
  , randomString
  ) where

import Control.Monad (replicateM)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as T
import System.Random.Stateful

import Data.ByteString qualified as BS

chunksOfBs :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfBs x = unfoldr (nothingWhen BS.null (BS.splitAt x))

justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)

randomString :: Int -> IO Text
randomString n = T.pack <$> replicateM n (uniformRM ('a', 'z') globalStdGen)
