module Options
  ( Options (..)
  , Command (..)
  , PeerAddress (..)
  , options
  ) where

import Control.Monad (void)
import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Text (Text)
import Data.Text qualified as T
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Options.Applicative
import Options.Applicative qualified as Opt

import Util (attoReadM)

newtype Options = Options
  { command :: Command
  }
  deriving (Show)

data Command
  = DecodeCommand Text
  | InfoCommand FilePath
  | PeersCommand FilePath
  | HandshakeCommand FilePath PeerAddress
  deriving (Show)

data PeerAddress = PeerAddress
  { ip :: IPv4
  , port :: Int
  }
  deriving (Show)

parsePeerAddress :: A.Parser PeerAddress
parsePeerAddress =
  ( do
      ip <- IPv4.parserUtf8
      void $ A.char ':'
      port <- A.decimal
      pure PeerAddress {..}
  ) <?> "peer address"

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
          <> Opt.command
            "handshake"
            ( info
                handshakeCommand
                ( progDesc
                    "Establish a TCP connection with a peer and complete a \
                    \handshake"
                )
            )
      )

decodeCommand :: Parser Command
decodeCommand = DecodeCommand <$> strArgument (metavar "BENCODE")

infoCommand :: Parser Command
infoCommand = InfoCommand . T.unpack <$> strArgument (metavar "FILENAME")

peersCommand :: Parser Command
peersCommand = PeersCommand . T.unpack <$> strArgument (metavar "FILENAME")

handshakeCommand :: Parser Command
handshakeCommand =
  HandshakeCommand . T.unpack
    <$> strArgument (metavar "FILENAME")
    <*> argument (attoReadM parsePeerAddress) (metavar "<peer_ip>:<peer_port>")
