module ParserSpec where

import Data.Word8 qualified as W
import Parser
import Test.Hspec

parserSpec :: Spec
parserSpec = do
  it "whitespace [single]" $ do
    runParser ws " foobar" `shouldBe` Just ("", "foobar")
    runParser ws "\nfoobar" `shouldBe` Just ("", "foobar")
    runParser ws "\rfoobar" `shouldBe` Just ("", "foobar")
    runParser ws "\tfoobar" `shouldBe` Just ("", "foobar")

  it "whitespace [multiple]" $ do
    runParser ws " \nfoobar" `shouldBe` Just ("", "foobar")
    runParser ws "\n\rfoobar" `shouldBe` Just ("", "foobar")
    runParser ws "\r\tfoobar" `shouldBe` Just ("", "foobar")
    runParser ws "\t foobar" `shouldBe` Just ("", "foobar")

  it "comma" $ do
    runParser comma ",foobar" `shouldBe` Just (W._comma, "foobar")
    runParser comma ",,foobar" `shouldBe` Just (W._comma, ",foobar")

  it "brackets" $ do
    runParser bracketL "[" `shouldBe` Just (W._bracketleft, "")
    runParser bracketL "]" `shouldBe` Nothing
    runParser bracketR "]" `shouldBe` Just (W._bracketright, "")
    runParser bracketR "[" `shouldBe` Nothing

  it "braces" $ do
    runParser braceL "{" `shouldBe` Just (W._braceleft, "")
    runParser braceL "}" `shouldBe` Nothing
    runParser braceR "}" `shouldBe` Just (W._braceright, "")
    runParser braceR "{" `shouldBe` Nothing

  it "quoteDouble" $ do
    runParser bracketL "[]" `shouldBe` Just (W._bracketleft, "]")
    runParser bracketR "]" `shouldBe` Just (W._bracketright, "")

  it "hyphen" $ do
    runParser hyphen "-123" `shouldBe` Just (W._hyphen, "123")
    runParser hyphen "123" `shouldBe` Nothing

  it "optionMaybe" $ do
    runParser (optionMaybe hyphen) "-42" `shouldBe` Just (Just W._hyphen, "42")
    runParser (optionMaybe hyphen) "42" `shouldBe` Just (Nothing, "42")

  it "sepBy" $ do
    runParser (sepBy comma (char W._a)) "" `shouldBe` Just ([], "")
    runParser (sepBy comma (char W._a)) "a" `shouldBe` Just ([W._a], "")
    runParser (sepBy comma (char W._a)) "a,a" `shouldBe` Just ([W._a, W._a], "")
    runParser (sepBy comma (char W._a)) "a,ab" `shouldBe` Just ([W._a, W._a], "b")
    runParser (sepBy comma jsonNumber) "1,2," `shouldBe` Nothing

  it "jsonBool" $ do
    runParser jsonBool "false" `shouldBe` Just (JBool False, "")
    runParser jsonBool "false!" `shouldBe` Just (JBool False, "!")
    runParser jsonBool "alse" `shouldBe` Nothing
    runParser jsonBool "true" `shouldBe` Just (JBool True, "")
    runParser jsonBool "true!" `shouldBe` Just (JBool True, "!")
    runParser jsonBool "rue" `shouldBe` Nothing

  it "jsonNull" $ do
    runParser jsonNull "nullfoo" `shouldBe` Just (JNull, "foo")
    runParser jsonNull "ull" `shouldBe` Nothing

  it "jsonNumber" $ do
    runParser jsonNumber "123" `shouldBe` Just (JNumber 123, "")
    runParser jsonNumber "-123" `shouldBe` Just (JNumber (-123), "")
    runParser jsonNumber "123foo" `shouldBe` Just (JNumber 123, "foo")

  it "jsonString" $ do
    runParser jsonString "\"foo\"" `shouldBe` Just (JString "foo", "")
    runParser jsonString "\"foo" `shouldBe` Nothing
    runParser jsonString "" `shouldBe` Nothing
    runParser jsonString "\"foo\\bar\"" `shouldBe` Just (JString "foo\\bar", "")
    runParser jsonString "\"foo\\bbar\"" `shouldBe` Just (JString "foo\\bbar", "")
    runParser jsonString "\"foo\\fbar\"" `shouldBe` Just (JString "foo\\fbar", "")
    runParser jsonString "\"foo\\nbar\"" `shouldBe` Just (JString "foo\\nbar", "")
    runParser jsonString "\"foo\\rbar\"" `shouldBe` Just (JString "foo\\rbar", "")
    runParser jsonString "\"foo\\tbar\"" `shouldBe` Just (JString "foo\\tbar", "")
    runParser jsonString "\"foo\\u0000bar\"" `shouldBe` Just (JString "foo\\u0000bar", "")

  it "jsonArray" $ do
    runParser jsonArray "[]" `shouldBe` Just (JArray [], "")
    runParser jsonArray "[1,2,3]" `shouldBe` Just (JArray [JNumber 1, JNumber 2, JNumber 3], "")
    runParser jsonArray "[1,2,]" `shouldBe` Nothing

  it "jsonObject" $ do
    runParser jsonObject "{}" `shouldBe` Just (JObject [], "")
    runParser jsonObject "{\"foo\": 42}" `shouldBe` Just (JObject [("foo", JNumber 43)], "")
    runParser jsonObject "{\"foo\"}" `shouldBe` Nothing
