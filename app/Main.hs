module Main where

import Data.ByteString qualified as B
import Data.Text.IO qualified as TIO
import Options.Applicative
import Parser (ParserError (..), parseJson)
import Prettify (format)
import Types

main :: IO ()
main = do
  options <- execParser opts
  bsContent <-
    case optInput options of
      FileInput filePath -> B.readFile filePath
      StdIn -> B.getContents
  case parseJson bsContent of
    Right json -> TIO.putStr (format (optSpacing options) json)
    Left (ParserError err) -> B.putStr err
  where
    opts =
      info
        (appOptions <**> helper)
        ( fullDesc
            <> progDesc "Parse and format JSON input"
            <> header "jqq - a simple 'jq' clone"
        )

appOptions :: Parser Options
appOptions =
  Options
    <$> spacingOpt
    <*> inputOpt
    <*> debugOpt

spacingOpt :: Parser Int
spacingOpt =
  option
    positiveInt
    ( long "spacing"
        <> help "How many spaces should we use?"
        <> showDefault
        <> value 2
        <> metavar "INT"
    )

positiveInt :: ReadM Int
positiveInt =
  let checkPositive i
        | i > 0 = pure i
        | otherwise = readerError "Value must be greater than 0"
   in auto >>= checkPositive

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

debugOpt :: Parser Bool
debugOpt =
  switch
    ( long "debug"
        <> short 'd'
        <> help "show internal AST"
    )
