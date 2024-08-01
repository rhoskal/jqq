module Parser where

import Control.Applicative
  ( Alternative,
    empty,
    (<|>),
  )
import Control.Monad ((>=>))
import Data.ByteString qualified as B
import Data.ByteString.Char8 (readInteger)
import Data.Word (Word8)
import Data.Word8 qualified as W

-- | Combinators
char :: Word8 -> Parser Word8
char w = lexeme $ Parser $ \input ->
  let (bs, rest) = B.splitAt 1 input
   in if bs == B.singleton w
        then pure (w, rest)
        else empty

string :: B.ByteString -> Parser B.ByteString
string bs = lexeme $ Parser $ \input ->
  let (matched, rest) = B.splitAt (B.length bs) input
   in if matched == bs
        then pure (matched, rest)
        else empty

many1 :: (Word8 -> Bool) -> Parser B.ByteString
many1 predicate = lexeme $ Parser $ \input ->
  let (matched, rest) = B.span predicate input
   in if B.null matched
        then empty
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
    Just (matched, rest) -> pure (Just matched, rest)
    Nothing -> pure (Nothing, input)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = elements <|> pure []
  where
    elements = (:) <$> element <*> restElements
    restElements = (sep *> element >>= \e -> (e :) <$> restElements) <|> pure []

isSpace :: Word8 -> Bool
isSpace w =
  w == W._space
    || w == W._lf
    || w == W._cr
    || w == W._tab

ws :: Parser B.ByteString
ws = skipMany isSpace

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

comma :: Parser Word8
comma = char W._comma

bracketL :: Parser Word8
bracketL = char W._bracketleft

bracketR :: Parser Word8
bracketR = char W._bracketright

braceL :: Parser Word8
braceL = char W._braceleft

braceR :: Parser Word8
braceR = char W._braceright

quoteDouble :: Parser Word8
quoteDouble = char W._quotedbl

hyphen :: Parser Word8
hyphen = char W._hyphen

colon :: Parser Word8
colon = char W._colon

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
jsonObject = JObject <$> (braceL *> ws *> pairs <* ws <* braceR)
  where
    pairs :: Parser [(B.ByteString, JsonValue)]
    pairs = sepBy (ws *> comma <* ws) (ws *> pair <* ws)

    pair :: Parser (B.ByteString, JsonValue)
    pair =
      (\key _ value -> (key, value))
        <$> stringLiteral
        <*> (ws *> colon <* ws)
        <*> jsonValue

jsonArray :: Parser JsonValue
jsonArray = JArray <$> (bracketL *> ws *> elements <* ws <* bracketR)
  where
    elements :: Parser [JsonValue]
    elements = sepBy (ws *> comma <* ws) (ws *> jsonValue <* ws)

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
  lexeme
    ( jsonBool
        <|> jsonNull
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject
    )

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
