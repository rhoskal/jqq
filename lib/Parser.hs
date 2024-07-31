module Parser where

import Control.Applicative
  ( Alternative,
    empty,
    many,
    (<|>),
  )
import Control.Monad ((>=>))
import Data.ByteString qualified as B
import Data.ByteString.Char8 (readInteger)
import Data.Char (ord)
import Data.Word (Word8)

-- | Combinators

char :: Char -> Parser B.ByteString
char c = Parser $ \input ->
  let (c', rest) = B.splitAt 1 input
   in if c' == B.singleton (fromIntegral $ ord c)
        then pure (c', rest)
        else empty

string :: B.ByteString -> Parser B.ByteString
string bs = Parser $ \input ->
  let (matched, rest) = B.splitAt (B.length bs) input
   in if matched == bs
        then pure (matched, rest)
        else empty

many1 :: (Word8 -> Bool) -> Parser B.ByteString
many1 predicate = Parser $ \input ->
  let (matched, rest) = B.span predicate input
   in if B.null matched
        then empty
        else pure (matched, rest)

spanP :: (Word8 -> Bool) -> Parser B.ByteString
spanP predicate = Parser $ \input ->
  let (matched, rest) = B.span predicate input
   in pure (matched, rest)

optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \input ->
  case p input of
    Just (matched, rest) -> pure (Just matched, rest)
    Nothing -> pure (Nothing, input)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element)

digit :: Word8 -> Bool
digit word = word >= 48 && word <= 57

space :: Word8 -> Bool
space word =
  word == 32 {- space -}
    || word == 10 {- line feed-}
    || word == 13 {- carriage return -}
    || word == 9 {- horizontal tab -}

ws :: Parser B.ByteString
ws = spanP space

-- | Parsers
data JsonValue
  = JBool !Bool
  | JNull
  | JNumber !Integer
  | JString !B.ByteString
  | JArray ![JsonValue]
  | JObject ![(B.ByteString, JsonValue)]
  deriving (Eq, Show)

jsonObject :: Parser JsonValue
jsonObject = JObject <$> (char '{' *> pairs <* char '}')
  where
    pairs :: Parser [(B.ByteString, JsonValue)]
    pairs = pure []

jsonArray :: Parser JsonValue
jsonArray = JArray <$> (char '[' *> ws *> elements <* ws <* char ']')
  where
    elements :: Parser [JsonValue]
    elements = sepBy (ws *> char ',' <* ws) jsonValue

jsonString :: Parser JsonValue
jsonString = JString <$> (char '"' *> stringLiteral <* char '"')
  where
    stringLiteral :: Parser B.ByteString
    stringLiteral = spanP (/= 34)

jsonNumber :: Parser JsonValue
jsonNumber = Parser $ \input -> do
  let Parser parseMinus = optional (char '-')
  let Parser parseDigits = many1 digit
  (minusSign, rest) <- parseMinus input
  (digits, rest') <- parseDigits rest
  let numStr = maybe digits (`B.append` digits) minusSign
  (num, _) <- readInteger numStr :: Maybe (Integer, B.ByteString)
  pure (JNumber num, rest')

jsonNull :: Parser JsonValue
jsonNull = Parser $ \input -> do
  let Parser p = string "null"
   in p input >>= (\(_, rest) -> pure (JNull, rest))

jsonBool :: Parser JsonValue
jsonBool =
  JBool . const True <$> string "true"
    <|> JBool . const False <$> string "false"

jsonValue :: Parser JsonValue
jsonValue =
  jsonBool
    <|> jsonNull
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

parse :: B.ByteString -> Maybe JsonValue
parse input = fst <$> runParser jsonValue input

-- | Custom Parser type
newtype Parser a = Parser
  { runParser :: B.ByteString -> Maybe (a, B.ByteString)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn (Parser p) =
    Parser (p >=> (\(matched, rest) -> pure (fn matched, rest)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \_ -> Just (a, B.empty)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p) =
    Parser $ \input ->
      case pf input of
        Just (fn, rest) ->
          case p rest of
            Just (matched, rest') -> pure (fn matched, rest')
            Nothing -> empty
        Nothing -> empty

  (*>) :: Parser a -> Parser b -> Parser b
  (Parser p1) *> (Parser p2) =
    Parser $ \input ->
      case p1 input of
        Just (_, rest) -> p2 rest
        Nothing -> empty

  (<*) :: Parser a -> Parser b -> Parser a
  (Parser p1) <* (Parser p2) =
    Parser $ \input ->
      case p1 input of
        Just (matched, rest) ->
          case p2 rest of
            Nothing -> empty
            Just (_, rest') -> pure (matched, rest')
        Nothing -> empty

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      case p1 input of
        Just result -> pure result
        Nothing -> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= fn = Parser $ \input ->
    case p1 input of
      Just (matched, rest) ->
        let Parser p2 = fn matched
         in p2 rest
      Nothing -> empty

  return :: a -> Parser a
  return = pure

