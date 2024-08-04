module Main (main) where

import ParserSpec (parserSpec)
import PrettySpec (prettySpec)
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  describe "Parser" parserSpec
  describe "Pretty" prettySpec
