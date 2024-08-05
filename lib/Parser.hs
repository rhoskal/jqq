module Parser where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad ((>=>))
import Data.ByteString qualified as B
import Data.Word (Word8)
import Data.Word8 qualified as W

-- Char Parsers

-- | Parses a single word `w`. Returns the parsed word (i.e. `w`).
char :: Word8 -> Parser Word8
char w =
  Parser $ \input ->
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

-- | Parses a sequence of words given by `bs`. Returns the parsed `ByteString`.
string :: B.ByteString -> Parser B.ByteString
string bs =
  Parser $ \input ->
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

-- | Succeeds for any word for which the supplied predicate returns `True`.
-- Returns the word that is actually parsed.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy predicate =
  Parser $ \input ->
    case B.uncons input of
      Nothing -> Left (ParserError "Unable to match on empty string")
      Just (w, rest) ->
        if predicate w
          then pure (w, rest)
          else
            Left $
              ParserError
                ( "Predicate failed with: "
                    <> mconcat ["'", B.singleton w, "'"]
                )

-- | Parses an ASCII digit (0-9).
digit :: Parser Word8
digit = satisfy W.isDigit

-- Combinators

-- | Returns the longest (non-empty) `ByteString` satisfying the given predicate.
many1 :: (Word8 -> Bool) -> Parser B.ByteString
many1 predicate =
  Parser $ \input ->
    let (matched, rest) = B.span predicate input
     in if B.null matched
          then Left (ParserError "Failed to match at least 1")
          else pure (matched, rest)

-- | Returns the longest (possibly empty) `ByteString` satisfying the given predicate.
manyTill :: (Word8 -> Bool) -> Parser B.ByteString
manyTill predicate =
  Parser $ \input ->
    let (matched, rest) = B.span predicate input
     in pure (matched, rest)

-- | Drops the longest (possibly empty) prefix of elements satisfying the predicate.
skipMany :: (Word8 -> Bool) -> Parser ()
skipMany predicate =
  Parser $ \input ->
    let rest = B.dropWhile predicate input
     in pure ((), rest)

-- | Attempts to apply parser `p`. If `p` fails without consuming input, it return `Nothing`,
-- otherwise it returns `Just` the value returned by `p`.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe (Parser p) =
  Parser $ \input ->
    case p input of
      Left _ -> pure (Nothing, input)
      Right (matched, rest) -> pure (Just matched, rest)

-- | Attempts to apply parser `p`. If `p` fails without consuming input, it returns the given `defaultValue`,
-- otherwise the value returned by `p`.
option :: a -> Parser a -> Parser a
option defaultValue p = p <|> return defaultValue

-- | Parses /zero/ or more occurrences of `p`, separated by `sep`. Returns a list of values returned by `p`.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  sepBy1 p sep <|> pure []

-- | Parses /one/ or more occurrences of `p`, separated by `sep`. Returns a list of values returned by `p`.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep =
  ((:) <$> p <*> many (sep *> p)) <?> "Failed to match at least 1"

-- | Only succeeds when parser `p` fails. This parser does not consume any input.
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \input ->
  case p input of
    Left _ -> pure ((), input)
    Right _ -> Left (ParserError "Unable to continue due to predicate success")

-- | Whitespace consumer.
ws :: Parser ()
ws =
  let isSpace :: Word8 -> Bool
      isSpace w =
        w == W._space
          || w == W._lf
          || w == W._cr
          || w == W._tab
   in skipMany isSpace

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

comma, bracketL, bracketR, braceL, braceR, quoteDouble, minus, plus, colon, period, e :: Parser Word8
comma = char W._comma
bracketL = char W._bracketleft
bracketR = char W._bracketright
braceL = char W._braceleft
braceR = char W._braceright
quoteDouble = char W._quotedbl
minus = char W._hyphen
plus = char W._plus
colon = char W._colon
period = char W._period
e = char W._e <|> char W._E

digits :: Parser B.ByteString
digits = many1 W.isDigit

-- Parsers

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
jsonObject =
  let pairs :: Parser [(B.ByteString, JsonValue)]
      pairs = sepBy pair (ws *> comma <* ws)

      pair :: Parser (B.ByteString, JsonValue)
      pair = liftA2 (,) (stringLiteral <* ws <* colon <* ws) jsonValue
   in JObject <$> (braceL *> ws *> pairs <* ws <* braceR)

jsonArray :: Parser JsonValue
jsonArray =
  let elements :: Parser [JsonValue]
      elements = sepBy jsonValue (ws *> comma <* ws)
   in JArray <$> (bracketL *> ws *> elements <* ws <* bracketR)

jsonString :: Parser JsonValue
jsonString =
  JString <$> stringLiteral

stringLiteral :: Parser B.ByteString
stringLiteral =
  quoteDouble *> manyTill (/= W._quotedbl) <* quoteDouble

integerPart :: Parser [Word8]
integerPart = do
  firstDigit <- digit
  if firstDigit == W._0
    then do
      _ <- notFollowedBy digit <?> "Leading zeros in integer literals are not permitted"
      return [firstDigit]
    else do
      restDigits <- many digit
      return (firstDigit : restDigits)

fractionPart :: Parser Double
fractionPart = do
  _ <- period
  mantissa <- some digit <?> "Expected at least 1 digit (0-9) following the '.'"
  let num = foldl (\acc x -> acc * 10 + fromIntegral (x - 48)) 0 mantissa
      len = length mantissa
  return (num / (10 ^ len))

exponentPart :: Parser Double
exponentPart = do
  _ <- e
  maybeSign <- optionMaybe (plus <|> minus)
  exponent' <- some digit
  let num = foldl (\acc x -> acc * 10 + fromIntegral (x - 48)) 0 exponent'
      factor = case maybeSign of
        Just w -> if w == W._hyphen then -num else num
        Nothing -> num
  return (10 ** factor)

jsonNumber :: Parser JsonValue
jsonNumber = do
  maybeMinus <- optionMaybe minus
  intPart <- integerPart
  fracPart <- option 0.0 fractionPart
  expPart <- option 1.0 exponentPart
  let base = foldl (\acc x -> acc * 10 + fromIntegral (x - 48)) 0 intPart
      result = case maybeMinus of
        Nothing -> base + fracPart
        Just _ -> (-1) * (base + fracPart)
      finalResult = result * expPart
  return $
    JNumber $
      if (floor finalResult :: Integer) == (ceiling finalResult :: Integer)
        then JInt $ round finalResult
        else JFloat finalResult

jsonBool :: Parser JsonValue
jsonBool =
  let jsonTrue :: Parser JsonValue
      jsonTrue = JBool True <$ string "true"

      jsonFalse :: Parser JsonValue
      jsonFalse = JBool False <$ string "false"
   in (jsonTrue <|> jsonFalse) <?> "Expected either 'true' or 'false'"

jsonNull :: Parser JsonValue
jsonNull =
  JNull <$ string "null"

jsonValue :: Parser JsonValue
jsonValue =
  lexeme
    ( jsonNull
        <|> jsonBool
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject
    )

parseJson :: B.ByteString -> Either ParserError JsonValue
parseJson input =
  fst <$> runParser jsonValue input

-- | Behaves as parser `p`, but whenever the parser `p` fails without consuming any input,
-- it replaces expect error messages with the expect error message `msg`.
(<?>) :: Parser a -> B.ByteString -> Parser a
(Parser p) <?> msg =
  Parser $ \input ->
    case p input of
      Left _ -> Left (ParserError msg)
      Right result -> Right result

newtype ParserError = ParserError B.ByteString
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: B.ByteString -> Either ParserError (a, B.ByteString)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn (Parser p) =
    Parser (p >=> (\(matched, rest) -> pure (fn matched, rest)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a =
    Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (fn, rest) <- p1 input
      (matched, rest') <- p2 rest
      pure (fn matched, rest')

instance Alternative (Either ParserError) where
  empty :: Either ParserError a
  empty =
    Left $ ParserError "empty"

  (<|>) :: Either ParserError a -> Either ParserError a -> Either ParserError a
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty :: Parser a
  empty =
    Parser $ const empty

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= fn =
    Parser $ \input -> do
      (matched, rest) <- p1 input
      let Parser p2 = fn matched
      p2 rest

  return :: a -> Parser a
  return = pure
