module PrettifySpec where

import Parser (JNum (..), JsonValue (..))
import Prettify
import Test.Hspec

prettifySpec :: Spec
prettifySpec = do
  let indentLevel :: Int = 2

  it "null" $ do
    format indentLevel JNull `shouldBe` "null"

  it "bool" $ do
    format indentLevel (JBool True) `shouldBe` "true"
    format indentLevel (JBool False) `shouldBe` "false"

  it "string" $ do
    format indentLevel (JString "foobar") `shouldBe` "\"foobar\""

  it "number" $ do
    format indentLevel (JNumber (JInt (-42))) `shouldBe` "-42"
    format indentLevel (JNumber (JInt 42)) `shouldBe` "42"
    format indentLevel (JNumber (JFloat (-42.0))) `shouldBe` "-42.0"
    format indentLevel (JNumber (JFloat 42.0)) `shouldBe` "42.0"

  it "array" $ do
    let json = JArray [JNumber (JInt 1), JNull, JBool True, JBool False]

    format indentLevel json `shouldBe` "[\n  1,\n  null,\n  true,\n  false\n]"

  it "object" $ do
    let json = JObject [("foo", JNumber (JInt 42)), ("bar", JNull)]

    format indentLevel json `shouldBe` "{\n  \"foo\": 42,\n  \"bar\": null\n}"
