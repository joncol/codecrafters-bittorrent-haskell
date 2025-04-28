module Util
  ( chunksOfBs
  ) where

import Data.List (unfoldr)

import qualified Data.ByteString as BS

chunksOfBs :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfBs x = unfoldr (nothingWhen BS.null (BS.splitAt x))

justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)
