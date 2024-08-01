module Main (main) where

import FormatterSpec (formatterSpec)
import ParserSpec (parserSpec)
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Parser" parserSpec
  describe "Formatter" formatterSpec
