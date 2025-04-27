import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy qualified as LB
import Data.Either (fromRight)
import Options.Applicative (execParser)
import System.IO

import Bencode.Parser
import Options

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  opts <- execParser options

  case opts.command of
    Decode encodedValue -> do
      let decodedValue =
            fromRight (error "parse error") $
              parseOnly parseBencodeValue (BSE.encode BSE.latin1 encodedValue)
          jsonValue = encode decodedValue
      LB.putStr jsonValue
      putStr "\n"
