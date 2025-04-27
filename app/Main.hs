import Control.Monad (when)
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.Either (fromRight)
import System.Environment
import System.Exit
import System.IO

import Bencode.Parser

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: your_bittorrent.sh <command> <args>"
    exitWith (ExitFailure 1)

  let command = args !! 0
  case command of
    "decode" -> do
      let encodedValue = args !! 1
      let decodedValue =
            fromRight (error "parse error") $
              parseOnly parseBencode (B.pack encodedValue)
      let jsonValue = encode decodedValue
      LB.putStr jsonValue
      putStr "\n"
    _ -> putStrLn $ "Unknown command: " ++ command
