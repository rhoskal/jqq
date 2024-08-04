module Prettify (format) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Parser (JNum (..), JsonValue (..))
import Prettyprinter ((<+>))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)

format :: Int -> JsonValue -> T.Text
format spacing = renderDoc . jsonValueToDoc spacing

renderDoc :: P.Doc ann -> T.Text
renderDoc = renderStrict . P.layoutPretty P.defaultLayoutOptions

jsonValueToDoc :: Int -> JsonValue -> P.Doc ann
jsonValueToDoc indentLevel value =
  case value of
    JNull -> P.pretty ("null" :: T.Text)
    JBool True -> P.pretty ("true" :: T.Text)
    JBool False -> P.pretty ("false" :: T.Text)
    JString str -> P.dquotes (P.pretty (TE.decodeUtf8Lenient str))
    JNumber (JInt num) -> P.pretty num
    JNumber (JFloat num) -> P.pretty num
    JArray values ->
      let formattedValues :: [P.Doc ann]
          formattedValues = map (jsonValueToDoc indentLevel) values
       in P.brackets $
            P.line
              <> P.indent indentLevel (P.vcat $ P.punctuate P.comma formattedValues)
              <> P.line
    JObject pairs ->
      let formattedPairs :: [P.Doc ann]
          formattedPairs = map formatPair pairs

          formatPair :: (B.ByteString, JsonValue) -> P.Doc ann
          formatPair (k, v) =
            formatKey k
              <> P.colon
              <+> jsonValueToDoc indentLevel v

          formatKey :: B.ByteString -> P.Doc ann
          formatKey = P.dquotes . P.pretty . BC.unpack
       in P.braces $
            P.line
              <> P.indent indentLevel (P.vcat $ P.punctuate P.comma formattedPairs)
              <> P.line
