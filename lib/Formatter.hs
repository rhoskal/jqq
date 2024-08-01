module Formatter (format) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Parser (JsonValue (..))

format :: JsonValue -> B.ByteString
format (JBool True) = "true"
format (JBool False) = "false"
format JNull = "null"
format (JString bs) = bs
format (JNumber num) = BC.pack $ show num
format (JArray arr) =
  "[ "
    <> B.intercalate ", " (map format arr)
    <> " ]"
format (JObject pairs) =
  "{\n"
    <> foldl (\acc pair -> acc <> fst pair <> ":" <> format (snd pair)) "" pairs
    <> "\n}"
