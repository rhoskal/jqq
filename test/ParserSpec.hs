module ParserSpec where

import Data.Word8 qualified as W
import Parser
import Test.Hspec

parserSpec :: Spec
parserSpec = do
  it "char" $ do
    runParser (char W._colon) "foobar" `shouldBe` Left (ParserError "Exepected ':' but found 'f'")
    runParser (char W._colon) ":foobar" `shouldBe` Right (W._colon, "foobar")

  it "string" $ do
    runParser (string "foo") "bar" `shouldBe` Left (ParserError "Expected 'foo' but found 'bar'")
    runParser (string "foo") "foobar" `shouldBe` Right ("foo", "bar")

  it "satisfy" $ do
    runParser (satisfy (== W._e)) "a" `shouldBe` Left (ParserError "Predicate failed with: 'a'")
    runParser (satisfy (== W._e)) "e" `shouldBe` Right (101, "")

  it "whitespace [single]" $ do
    runParser ws " foobar" `shouldBe` Right ((), "foobar")
    runParser ws "\nfoobar" `shouldBe` Right ((), "foobar")
    runParser ws "\rfoobar" `shouldBe` Right ((), "foobar")
    runParser ws "\tfoobar" `shouldBe` Right ((), "foobar")

  it "whitespace [multiple]" $ do
    runParser ws " \nfoobar" `shouldBe` Right ((), "foobar")
    runParser ws "\n\rfoobar" `shouldBe` Right ((), "foobar")
    runParser ws "\r\tfoobar" `shouldBe` Right ((), "foobar")
    runParser ws "\t foobar" `shouldBe` Right ((), "foobar")

  it "comma" $ do
    runParser comma ",foobar" `shouldBe` Right (W._comma, "foobar")
    runParser comma ",,foobar" `shouldBe` Right (W._comma, ",foobar")

  it "brackets" $ do
    runParser bracketL "[" `shouldBe` Right (W._bracketleft, "")
    runParser bracketL "]" `shouldBe` Left (ParserError "Exepected '[' but found ']'")
    runParser bracketR "]" `shouldBe` Right (W._bracketright, "")
    runParser bracketR "[" `shouldBe` Left (ParserError "Exepected ']' but found '['")

  it "braces" $ do
    runParser braceL "{" `shouldBe` Right (W._braceleft, "")
    runParser braceL "}" `shouldBe` Left (ParserError "Exepected '{' but found '}'")
    runParser braceR "}" `shouldBe` Right (W._braceright, "")
    runParser braceR "{" `shouldBe` Left (ParserError "Exepected '}' but found '{'")

  it "quoteDouble" $ do
    runParser quoteDouble "\"" `shouldBe` Right (W._quotedbl, "")
    runParser quoteDouble "" `shouldBe` Left (ParserError "Exepected '\"' but found ''")

  it "optionMaybe" $ do
    runParser (optionMaybe minus) "-42" `shouldBe` Right (Just W._hyphen, "42")
    runParser (optionMaybe minus) "42" `shouldBe` Right (Nothing, "42")

  it "minus" $ do
    runParser minus "-123" `shouldBe` Right (W._hyphen, "123")
    runParser minus "123" `shouldBe` Left (ParserError "Exepected '-' but found '1'")

  it "many1" $ do
    runParser (many1 W.isDigit) "" `shouldBe` Left (ParserError "Failed to match at least 1")
    runParser (many1 W.isDigit) "42a" `shouldBe` Right ("42", "a")

  it "sepBy" $ do
    runParser (sepBy jsonNull comma) "" `shouldBe` Right ([], "")
    runParser (sepBy jsonNull comma) "null" `shouldBe` Right ([JNull], "")
    runParser (sepBy jsonNull comma) "null,null" `shouldBe` Right ([JNull, JNull], "")
    runParser (sepBy jsonNumber comma) "1,2," `shouldBe` Right ([JNumber (JInt 1), JNumber (JInt 2)], ",")
    runParser (sepBy jsonNull comma) "null,nullx" `shouldBe` Right ([JNull, JNull], "x")

  it "sepBy1" $ do
    runParser (sepBy1 jsonNumber (ws *> comma <* ws)) "1,2, 3" `shouldBe` Right ([JNumber (JInt 1), JNumber (JInt 2), JNumber (JInt 3)], "")
    runParser (sepBy1 jsonNumber (ws *> comma <* ws)) "1" `shouldBe` Right ([JNumber (JInt 1)], "")
    runParser (sepBy1 jsonNumber (ws *> comma <* ws)) "" `shouldBe` Left (ParserError "Failed to match at least 1")

  it "jsonBool" $ do
    runParser jsonBool "false" `shouldBe` Right (JBool False, "")
    runParser jsonBool "false!" `shouldBe` Right (JBool False, "!")
    runParser jsonBool "alse" `shouldBe` Left (ParserError "Expected either 'true' or 'false'")
    runParser jsonBool "true" `shouldBe` Right (JBool True, "")
    runParser jsonBool "true!" `shouldBe` Right (JBool True, "!")
    runParser jsonBool "rue" `shouldBe` Left (ParserError "Expected either 'true' or 'false'")

  it "jsonNull" $ do
    runParser jsonNull "nullfoo" `shouldBe` Right (JNull, "foo")
    runParser jsonNull "ull" `shouldBe` Left (ParserError "Expected 'null' but found 'ull'")

  it "jsonNumber" $ do
    runParser jsonNumber "0" `shouldBe` Right (JNumber (JInt 0), "")
    runParser jsonNumber "01" `shouldBe` Left (ParserError "Predicate failed with: '0'")
    runParser jsonNumber "123" `shouldBe` Right (JNumber (JInt 123), "")
    runParser jsonNumber "-123" `shouldBe` Right (JNumber (JInt (-123)), "")
    runParser jsonNumber "123foo" `shouldBe` Right (JNumber (JInt 123), "foo")
    runParser jsonNumber "0.0" `shouldBe` Right (JNumber (JFloat 0.0), "")
    runParser jsonNumber "-0.0" `shouldBe` Right (JNumber (JFloat (-0.0)), "")
    runParser jsonNumber "0.1e3" `shouldBe` Right (JNumber (JFloat 100.0), "")
    runParser jsonNumber "0.1e+3" `shouldBe` Right (JNumber (JFloat 100.0), "")
    runParser jsonNumber "0.1e-3" `shouldBe` Right (JNumber (JFloat 0.0001), "")
    runParser jsonNumber "0.0e3" `shouldBe` Left (ParserError "bad format")
    runParser jsonNumber "01.0" `shouldBe` Left (ParserError "bad format")
    runParser jsonNumber "1.e" `shouldBe` Left (ParserError "bad format")

  it "jsonString" $ do
    runParser jsonString "\"foo\"" `shouldBe` Right (JString "foo", "")
    runParser jsonString "\"foo" `shouldBe` Left (ParserError "Exepected '\"' but found ''")
    runParser jsonString "" `shouldBe` Left (ParserError "Exepected '\"' but found ''")
    runParser jsonString "\"foo\\bar\"" `shouldBe` Right (JString "foo\\bar", "")
    runParser jsonString "\"foo\\bbar\"" `shouldBe` Right (JString "foo\\bbar", "")
    runParser jsonString "\"foo\\fbar\"" `shouldBe` Right (JString "foo\\fbar", "")
    runParser jsonString "\"foo\\nbar\"" `shouldBe` Right (JString "foo\\nbar", "")
    runParser jsonString "\"foo\\rbar\"" `shouldBe` Right (JString "foo\\rbar", "")
    runParser jsonString "\"foo\\tbar\"" `shouldBe` Right (JString "foo\\tbar", "")
    runParser jsonString "\"foo\\u0000bar\"" `shouldBe` Right (JString "foo\\u0000bar", "")

  xit "jsonArray" $ do
    runParser jsonArray "[]" `shouldBe` Right (JArray [], "")
    runParser jsonArray "[1,2,3]" `shouldBe` Right (JArray [JNumber (JInt 1), JNumber (JInt 2), JNumber (JInt 3)], "")
    runParser jsonArray "[1,2,]" `shouldBe` Left (ParserError "errrrrrr")

  xit "jsonObject" $ do
    runParser jsonObject "{}" `shouldBe` Right (JObject [], "")
    runParser jsonObject "{\"foo\": 42}" `shouldBe` Right (JObject [("foo", JNumber (JInt 43))], "")
    runParser jsonObject "{\"foo\"}" `shouldBe` Left (ParserError "errrrrrr")
