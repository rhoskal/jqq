module Main where

import Data.ByteString qualified as B
import Formatter
import Parser (ParserError (..), parse)

main :: IO ()
main = do
  case parse "true" of
    Right json -> B.putStr $ format json
    Left (ParserError err) -> B.putStr err
