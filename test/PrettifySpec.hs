module PrettifySpec where

import Parser (JNum (..), JsonValue (..))
import Prettify
import Test.Hspec
import Types (Indent (..))

prettifySpec :: Spec
prettifySpec = do
  let indentLevel = Indent 2

  it "null" $ do
    pretty indentLevel JNull `shouldBe` "null"

  it "bool" $ do
    pretty indentLevel (JBool True) `shouldBe` "true"
    pretty indentLevel (JBool False) `shouldBe` "false"

  it "string" $ do
    pretty indentLevel (JString "foobar") `shouldBe` "\"foobar\""

  it "number" $ do
    pretty indentLevel (JNumber (JInt (-42))) `shouldBe` "-42"
    pretty indentLevel (JNumber (JInt 42)) `shouldBe` "42"
    pretty indentLevel (JNumber (JFloat (-42.0))) `shouldBe` "-42.0"
    pretty indentLevel (JNumber (JFloat 42.0)) `shouldBe` "42.0"

  it "array" $ do
    let json = JArray [JNumber (JInt 1), JNull, JBool True, JBool False]

    pretty indentLevel json `shouldBe` "[\n  1,\n  null,\n  true,\n  false\n]"

  it "object" $ do
    let json = JObject [("foo", JNumber (JInt 42)), ("bar", JNull)]

    pretty indentLevel json `shouldBe` "{\n  \"foo\": 42,\n  \"bar\": null\n}"
