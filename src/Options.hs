module Options
  ( Options (..)
  , Command (..)
  , options
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative qualified as Opt

import Util (attoReadM)
import Torrent.PeerAddress

newtype Options = Options
  { command :: Command
  }
  deriving (Show)

data Command
  = DecodeCommand Text
  | InfoCommand FilePath
  | PeersCommand FilePath
  | HandshakeCommand FilePath PeerAddress
  | DownloadPieceCommand FilePath FilePath Int
  | DownloadCommand FilePath FilePath
  | MagnetParseCommand Text
  | MagnetHandshakeCommand Text
  | MagnetInfoCommand Text
  | MagnetDownloadPieceCommand FilePath Text Int
  | MagnetDownloadCommand FilePath Text
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
          <> Opt.command
            "handshake"
            ( info
                handshakeCommand
                ( progDesc
                    "Establish a TCP connection with a peer and complete a \
                    \handshake"
                )
            )
          <> Opt.command
            "download_piece"
            ( info
                downloadPieceCommand
                (progDesc "Download a piece of a torrent")
            )
          <> Opt.command
            "download"
            ( info
                downloadCommand
                (progDesc "Download a torrent")
            )
          <> Opt.command
            "magnet_parse"
            ( info
                magnetParseCommand
                (progDesc "Parse a magnet link")
            )
          <> Opt.command
            "magnet_handshake"
            ( info
                magnetHandshakeCommand
                (progDesc
                    "Establish a TCP connection with a peer and complete a \
                    \handshake, using a magnet link"
                )
            )
          <> Opt.command
            "magnet_info"
            ( info
                magnetInfoCommand
                (progDesc
                    "Request metadata from peers, using a magnet link"
                )
            )
          <> Opt.command
            "magnet_download_piece"
            ( info
                magnetDownloadPieceCommand
                (progDesc "Download a piece of a torrent, using a magnet link")
            )
          <> Opt.command
            "magnet_download"
            ( info
                magnetDownloadCommand
                (progDesc "Download a torrent, using a magnet link")
            )
      )

decodeCommand :: Parser Command
decodeCommand = DecodeCommand <$> strArgument (metavar "BENCODE")

infoCommand :: Parser Command
infoCommand =
  InfoCommand . T.unpack <$> strArgument (metavar "TORRENT_FILENAME")

peersCommand :: Parser Command
peersCommand =
  PeersCommand . T.unpack <$> strArgument (metavar "TORRENT_FILENAME")

handshakeCommand :: Parser Command
handshakeCommand =
  HandshakeCommand . T.unpack
    <$> strArgument (metavar "TORRENT_FILENAME")
    <*> argument (attoReadM parsePeerAddress) (metavar "<peer_ip>:<peer_port>")

downloadPieceCommand :: Parser Command
downloadPieceCommand =
  DownloadPieceCommand . T.unpack
    <$> strOption (short 'o' <> metavar "OUTPUT_FILENAME")
    <*> strArgument (metavar "TORRENT_FILENAME")
    <*> argument auto (metavar "PIECE_INDEX")

downloadCommand :: Parser Command
downloadCommand =
  DownloadCommand . T.unpack
    <$> strOption (short 'o' <> metavar "OUTPUT_FILENAME")
    <*> strArgument (metavar "TORRENT_FILENAME")

magnetParseCommand :: Parser Command
magnetParseCommand = MagnetParseCommand <$> strArgument (metavar "MAGNET_LINK")

magnetHandshakeCommand :: Parser Command
magnetHandshakeCommand =
  MagnetHandshakeCommand <$> strArgument (metavar "MAGNET_LINK")

magnetInfoCommand :: Parser Command
magnetInfoCommand = MagnetInfoCommand <$> strArgument (metavar "MAGNET_LINK")

magnetDownloadPieceCommand :: Parser Command
magnetDownloadPieceCommand =
  MagnetDownloadPieceCommand . T.unpack
    <$> strOption (short 'o' <> metavar "OUTPUT_FILENAME")
    <*> strArgument (metavar "MAGNET_LINK")
    <*> argument auto (metavar "PIECE_INDEX")

magnetDownloadCommand :: Parser Command
magnetDownloadCommand =
  MagnetDownloadCommand . T.unpack
    <$> strOption (short 'o' <> metavar "OUTPUT_FILENAME")
    <*> strArgument (metavar "MAGNET_LINK")
