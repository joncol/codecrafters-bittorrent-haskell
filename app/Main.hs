import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Options.Applicative (execParser)
import System.IO

import App
import AppEnv
import AppMonad
import Options
import Util

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts <- execParser options
  myPeerId <- randomString 20
  let appEnv = AppEnv {myPeerId}

  mResult <-
    runExceptT
      . flip runReaderT appEnv
      . runAppM
      $ runCommand opts.command

  either (\err -> hPutStrLn stderr $ "error: " <> show err) pure mResult
