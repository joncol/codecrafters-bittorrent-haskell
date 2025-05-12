import Options.Applicative (execParser)
import System.IO

import App
import Options
import Util

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts <- execParser options
  myPeerId <- randomString 20
  runCommand myPeerId opts.command
