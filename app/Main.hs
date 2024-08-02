module Main where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Formatter
import Options.Applicative
import Parser (ParserError (..), parseJson)

data Options = Options
  { optSpacing :: !Int,
    optInput :: !B.ByteString
  }
  deriving (Eq, Show)

appOptions :: Parser Options
appOptions = Options <$> spacingOpt <*> inputOpt

spacingOpt :: Parser Int
spacingOpt =
  option
    auto
    ( long "spacing"
        <> help "How many spaces should we use?"
        <> showDefault
        <> value 2
        <> metavar "INT"
    )

inputOpt :: Parser B.ByteString
inputOpt =
  BC.pack
    <$> strOption
      ( long "input"
          <> short 'i'
          <> help "JSON string"
          <> metavar "JSON"
      )

main :: IO ()
main = do
  options <- execParser opts
  case parseJson (optInput options) of
    Right json -> B.putStr $ format json
    Left (ParserError err) -> B.putStr err
  where
    opts =
      info
        (appOptions <**> helper)
        ( fullDesc
            <> progDesc "Parse and format JSON input"
            <> header "jqq - a simple 'jq' clone"
        )
