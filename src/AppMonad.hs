module AppMonad
  ( AppM (..)
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader

import AppError

newtype AppM r m a = AppM {runAppM :: ReaderT r (ExceptT AppError m) a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError AppError
    , MonadIO
    , MonadReader r
    )
