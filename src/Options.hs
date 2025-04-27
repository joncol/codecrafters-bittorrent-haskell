module Options
  ( Options (..)
  , Command (..)
  , options
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative qualified as Opt

data Options = Options
  { command :: Command
  }
  deriving (Show)

data Command
  = Decode Text
  | Info FilePath
  deriving (Show)

options :: ParserInfo Options
options =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "CodeCrafters BitTorrent client challenge"
        <> header "BitTorrent client"
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> subparser
      ( Opt.command
          "decode"
          ( info
              decodeCommand
              (progDesc "Decode a given Bencode value")
          )
          <> Opt.command
            "info"
            ( info
                infoCommand
                (progDesc "Print information about torrent file")
            )
      )

decodeCommand :: Parser Command
decodeCommand = Decode <$> strArgument (metavar "BENCODE")

infoCommand :: Parser Command
infoCommand = Info . T.unpack <$> strArgument (metavar "FILENAME")
