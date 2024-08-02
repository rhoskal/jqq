module Parser where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad ((>=>))
import Data.ByteString qualified as B
import Data.ByteString.Char8 (readInteger)
import Data.Word (Word8)
import Data.Word8 qualified as W

-- | Combinators
char :: Word8 -> Parser Word8
char w = Parser $ \input ->
  let (slice, rest) = B.splitAt 1 input
   in if slice == B.singleton w
        then pure (w, rest)
        else
          Left $
            ParserError
              ( "Exepected "
                  <> mconcat ["'", B.singleton w, "'"]
                  <> " but found "
                  <> mconcat ["'", slice, "'"]
              )

string :: B.ByteString -> Parser B.ByteString
string bs = Parser $ \input ->
  let (slice, rest) = B.splitAt (B.length bs) input
   in if slice == bs
        then pure (slice, rest)
        else
          Left $
            ParserError
              ( "Expected "
                  <> mconcat ["'", bs, "'"]
                  <> " but found "
                  <> mconcat ["'", slice, "'"]
              )

many1 :: (Word8 -> Bool) -> Parser B.ByteString
many1 predicate = Parser $ \input ->
  let (matched, rest) = B.span predicate input
   in if B.null matched
        then Left (ParserError "Failed to match at least 1")
        else pure (matched, rest)

manyTill :: (Word8 -> Bool) -> Parser B.ByteString
manyTill predicate = Parser $ \input ->
  pure $ B.span predicate input

skipMany :: (Word8 -> Bool) -> Parser B.ByteString
skipMany predicate = Parser $ \input ->
  let rest = B.dropWhile predicate input
   in pure (B.empty, rest)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe (Parser p) = Parser $ \input ->
  case p input of
    Left _ -> pure (Nothing, input)
    Right (matched, rest) -> pure (Just matched, rest)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

isSpace :: Word8 -> Bool
isSpace w =
  w == W._space
    || w == W._lf
    || w == W._cr
    || w == W._tab

ws :: Parser B.ByteString
ws = skipMany isSpace

comma, bracketL, bracketR, braceL, braceR, quoteDouble, hyphen, colon :: Parser Word8
comma = char W._comma
bracketL = char W._bracketleft
bracketR = char W._bracketright
braceL = char W._braceleft
braceR = char W._braceright
quoteDouble = char W._quotedbl
hyphen = char W._hyphen
colon = char W._colon

-- | Parsers
data JsonValue
  = JBool !Bool
  | JNull
  | JNumber JNum
  | JString !B.ByteString
  | JArray ![JsonValue]
  | JObject ![(B.ByteString, JsonValue)]
  deriving (Eq, Show)

data JNum
  = JInt !Integer
  | JFloat !Double
  deriving (Eq, Show)

jsonObject :: Parser JsonValue
jsonObject = JObject <$> (braceL *> pairs <* braceR)
  where
    pairs :: Parser [(B.ByteString, JsonValue)]
    pairs = sepBy pair (ws *> comma <* ws)

    pair :: Parser (B.ByteString, JsonValue)
    pair = liftA2 (,) (stringLiteral <* ws <* colon <* ws) jsonValue

jsonArray :: Parser JsonValue
jsonArray = JArray <$> (bracketL *> elements <* bracketR)
  where
    elements :: Parser [JsonValue]
    elements = sepBy jsonValue (ws *> comma <* ws)

jsonString :: Parser JsonValue
jsonString = JString <$> stringLiteral

stringLiteral :: Parser B.ByteString
stringLiteral = quoteDouble *> manyTill (/= W._quotedbl) <* quoteDouble

jsonNumber :: Parser JsonValue
jsonNumber = Parser $ \input -> do
  let Parser parseMinus = optionMaybe (B.singleton <$> hyphen)
  let Parser parseDigits = many1 W.isDigit
  (minusSign, rest) <- parseMinus input
  (digits, rest') <- parseDigits rest
  let numStr = maybe digits (`B.append` digits) minusSign
  case (readInteger numStr :: Maybe (Integer, B.ByteString)) of
    Nothing ->
      Left $
        ParserError
          ( "Failed to convert "
              <> mconcat ["'", numStr, "'"]
              <> " to Integer"
          )
    Just (num, _) -> pure (JNumber $ JInt num, rest')

jsonBool :: Parser JsonValue
jsonBool = (jsonTrue <|> jsonFalse) <?> "Expected either 'true' or 'false'"
  where
    jsonTrue = JBool True <$ string "true"
    jsonFalse = JBool False <$ string "false"

jsonNull :: Parser JsonValue
jsonNull = JNull <$ string "null"

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

parseJson :: B.ByteString -> Either ParserError JsonValue
parseJson input = fst <$> runParser jsonValue input

(<?>) :: Parser a -> B.ByteString -> Parser a
(Parser p) <?> errMsg = Parser $ \input ->
  case p input of
    Left _ -> Left $ ParserError errMsg
    Right result -> Right result

newtype ParserError = ParserError B.ByteString
  deriving (Eq, Show)

-- | Custom Parser type
newtype Parser a = Parser
  { runParser :: B.ByteString -> Either ParserError (a, B.ByteString)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn (Parser p) =
    Parser (p >=> (\(matched, rest) -> pure (fn matched, rest)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \_ -> Right (a, B.empty)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (fn, rest) <- p1 input
      (matched, rest') <- p2 rest
      pure (fn matched, rest')

instance Alternative (Either ParserError) where
  empty :: Either ParserError a
  empty = Left $ ParserError "empty"

  (<|>) :: Either ParserError a -> Either ParserError a -> Either ParserError a
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const empty

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input
