module Pretty (format) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Parser (JNum (..), JsonValue (..))

format :: JsonValue -> B.ByteString
format JNull = "null"
format (JBool True) = "true"
format (JBool False) = "false"
format (JString str) = str
format (JNumber (JInt num)) = BC.pack $ show num
format (JNumber (JFloat num)) = BC.pack $ show num
format (JArray arr) =
  let values :: [JsonValue] -> B.ByteString
      values [] = ""
      values vs = B.intercalate ", " (map format vs)
   in "[" <> values arr <> "]"
format (JObject obj) =
  let pairs :: [(B.ByteString, JsonValue)] -> B.ByteString
      pairs [] = ""
      pairs ps = B.intercalate ", " (map formatPair ps)

      formatPair :: (B.ByteString, JsonValue) -> B.ByteString
      formatPair (k, v) = BC.pack (show k) <> ": " <> format v
   in "{" <> pairs obj <> "}"
