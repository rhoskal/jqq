module PrettifySpec where

import Parser (JNum (..), JsonValue (..))
import Prettify
import Test.Hspec

prettifySpec :: Spec
prettifySpec = do
  it "null" $ do
    format JNull `shouldBe` "null"

  it "bool" $ do
    format (JBool True) `shouldBe` "true"
    format (JBool False) `shouldBe` "false"

  it "string" $ do
    format (JString "foobar") `shouldBe` "foobar"

  it "number" $ do
    format (JNumber (JInt (-42))) `shouldBe` "-42"
    format (JNumber (JInt 42)) `shouldBe` "42"
    format (JNumber (JFloat (-42.0))) `shouldBe` "-42.0"
    format (JNumber (JFloat 42.0)) `shouldBe` "42.0"

  it "array" $ do
    let json = JArray [JNumber (JInt 1), JNull, JBool True, JBool False]

    format json `shouldBe` "[1, null, true, false]"

  it "object" $ do
    let json = JObject [("foo", JNumber (JInt 42)), ("bar", JNull)]

    format json `shouldBe` "{\"foo\": 42, \"bar\": null}"
