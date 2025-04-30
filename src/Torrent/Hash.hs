module Torrent.Hash
  ( Hash (..)
  )
where

import Data.Binary (Binary)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BSB16
import Data.ByteString.Encoding qualified as BSE
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype Hash = Hash {getHash :: BS.ByteString}
  deriving (Generic)

instance Binary Hash

instance Show Hash where
  show (Hash bs) = T.unpack . BSE.decode BSE.latin1 $ BSB16.encode bs
