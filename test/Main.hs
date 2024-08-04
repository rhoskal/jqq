module Main (main) where

import ParserSpec (parserSpec)
import PrettifySpec (prettifySpec)
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Parser" parserSpec
  describe "Prettify" prettifySpec
