module Main where

import Data.ByteString qualified as B
import Formatter
import Options.Applicative
import Parser (ParserError (..), parseJson)

data Options = Options
  { optSpacing :: !Int,
    optInput :: InputMode
  }
  deriving (Eq, Show)

data InputMode
  = FileInput !FilePath
  | StdIn
  deriving (Eq, Show)

appOptions :: Parser Options
appOptions =
  Options <$> spacingOpt <*> inputOpt

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

inputOpt :: Parser InputMode
inputOpt =
  let fileInput :: Parser InputMode
      fileInput =
        FileInput
          <$> strOption
            ( long "file"
                <> short 'f'
                <> metavar "FILE_PATH"
                <> help "Path to JSON file"
            )

      stdIn :: Parser InputMode
      stdIn =
        flag'
          StdIn
          ( long "stdin"
              <> help "Read JSON from stdin"
          )
   in fileInput <|> stdIn

main :: IO ()
main = do
  options <- execParser opts
  bsContent <-
    case optInput options of
      FileInput filePath -> B.readFile filePath
      StdIn -> B.getContents
  case parseJson bsContent of
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
