module AppEnv
  ( AppEnv (..)
  ) where

import Data.Text (Text)

newtype AppEnv = AppEnv
  { myPeerId :: Text
  }
  deriving (Show)
