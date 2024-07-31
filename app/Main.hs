module Main where

import Parser qualified (parse)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Parser.parse
