module Util
  ( chunksOfBs
  , randomString
  , randomBytes
  , attoReadM
  , byteF
  ) where

import Control.Monad (replicateM)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Fmt (Builder)
import Formatting qualified as F
import Options.Applicative
import System.Random.Stateful

chunksOfBs :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfBs x = unfoldr . nothingWhen BS.null $ BS.splitAt x

justWhen :: (a -> Bool) -> (a -> b) -> a -> Maybe b
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)

randomString :: Int -> IO Text
randomString n = T.pack <$> replicateM n (uniformRM ('a', 'z') globalStdGen)

randomBytes :: Int -> IO BS.ByteString
randomBytes n = BS.pack <$> replicateM n (uniformRM (0, 255) globalStdGen)

-- | Convert an 'attoparsec' parser to an 'optparse-applicative' custom reader.
attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . BS8.pack)

-- | Format a byte value as two hex digits.
-- See: https://chrisdone.com/posts/formatting
byteF :: Word8 -> Builder
byteF = F.bprint $ F.lpadded 2 '0' F.hex
