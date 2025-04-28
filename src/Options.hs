module Options
  ( Options (..)
  , Command (..)
  , options
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative qualified as Opt

newtype Options = Options
  { command :: Command
  }
  deriving (Show)

data Command
  = DecodeCommand Text
  | InfoCommand FilePath
  | PeersCommand FilePath
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
          <> Opt.command
            "peers"
            ( info
                peersCommand
                ( progDesc
                    "Discover peers by connecting to the tracker URL for \
                    \torrent file"
                )
            )
      )

decodeCommand :: Parser Command
decodeCommand = DecodeCommand <$> strArgument (metavar "BENCODE")

infoCommand :: Parser Command
infoCommand = InfoCommand . T.unpack <$> strArgument (metavar "FILENAME")

peersCommand :: Parser Command
peersCommand = PeersCommand . T.unpack <$> strArgument (metavar "FILENAME")
