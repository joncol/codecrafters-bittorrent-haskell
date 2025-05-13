module Messages.Unchoke
  ( Unchoke
  ) where

import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import GHC.Generics (Generic)

data Unchoke = Unchoke deriving (Generic, Show)

msgId :: Bin.Word8
msgId = 1

instance Binary Unchoke where
  get = do
    msgLen <- Bin.getWord32be
    guard $ msgLen == 1
    msgId' <- Bin.getWord8
    guard $ msgId' == msgId
    pure Unchoke
