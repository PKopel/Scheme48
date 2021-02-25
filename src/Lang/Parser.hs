{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lang.Parser where

import           Import                  hiding ( try )
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Sequence                  ( fromList )
import           Data.Complex                   ( Complex((:+)) )
import           Data.Ratio                     ( (%) )
import           RIO.Partial                    ( read )


data Base = Bin | Oct | Dec | Hex

oneOf :: [Char] -> Parser Char
oneOf list = satisfy (`elem` list)

noneOf :: [Char] -> Parser Char
noneOf list = satisfy (`notElem` list)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

skipSpaces :: Parser ()
skipSpaces = skipMany space

escaped :: Parser Char
escaped = char '\\' *> oneOf "\\\"\'nrt"

parseChar :: Parser LispVal
parseChar = "#\\" *> (name <|> letter <|> symbol <|> oneOf "\\\"\'") <&> Char
  where name = ("space" $> ' ') <|> ("newline" $> '\n')

parseString :: Parser LispVal
parseString =
  char '"' *> many (noneOf "\\\"" <|> escaped) <* char '"' <&> String

parseAtom :: Parser LispVal
parseAtom = do
  firstLetter <- letter <|> symbol
  rest        <- many (letter <|> digit <|> symbol)
  let atom = firstLetter : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

radixPref :: Parser Base
radixPref = char '#' *> (bin <|> oct <|> dec <|> hex)
 where
  bin = char 'b' $> Bin
  oct = char 'o' $> Oct
  dec = char 'd' $> Dec
  hex = char 'x' $> Hex

parseBase :: (Integral a, Read a) => a -> [Char] -> a
parseBase a list = parseBase' list 0
 where
  parseBase' []      n = n
  parseBase' (h : t) n = parseBase' t (n * a + parseDigit h)
  parseDigit c | c == 'A' || c == 'a' = 10
               | c == 'B' || c == 'b' = 11
               | c == 'C' || c == 'c' = 12
               | c == 'D' || c == 'd' = 13
               | c == 'E' || c == 'e' = 14
               | c == 'F' || c == 'f' = 15
               | otherwise            = read [c]

parseInteger :: Parser NumType
parseInteger = Integer <$> do
  pref <- option Dec radixPref
  case pref of
    Bin -> parseBase 2 <$> many1 digit
    Oct -> parseBase 8 <$> many1 digit
    Dec -> parseBase 10 <$> many1 digit
    Hex -> parseBase 16 <$> many1 digit

parseRational :: Parser NumType
parseRational = (%) <$> (decimal <* "/") <*> decimal <&> Rational

parseReal :: Parser NumType
parseReal = double <&> Real

parseComplex :: Parser NumType
parseComplex =
  (:+) <$> (double <* oneOf "+-") <*> (double <* char 'i') <&> Complex

parseNumber :: Parser LispVal
parseNumber =
  try parseInteger
    <|> try parseReal
    <|> try parseRational
    <|> try parseComplex
    <&> Number

parseList :: Parser LispVal -> Parser LispVal
parseList recParser = do
  char '(' *> skipSpaces
  inits <- sepBy recParser skipSpaces
  last  <- skipSpaces *> oneOf ".)"
  case last of
    ')' -> return $ List inits
    _ -> skipSpaces *> recParser <* skipSpaces <* char ')' <&> DottedList inits

parseVector :: Parser LispVal -> Parser LispVal
parseVector recParser =
  "#("
    *>  skipSpaces
    *>  sepBy recParser skipSpaces
    <*  skipSpaces
    <*  char ')'
    <&> Vector
    .   fromList

parseQuotes :: Text -> LispVal -> Parser LispVal -> Parser LispVal
parseQuotes c val recParser = do
  _ <- string c
  x <- recParser
  return $ List [val, x]

parseQuasiquote :: Parser LispVal -> Parser LispVal
parseQuasiquote = parseQuotes "`" (Atom "quasiquote")

parseQuote :: Parser LispVal -> Parser LispVal
parseQuote = parseQuotes "\'" (Atom "quote")

parseUnquote :: Parser LispVal -> Parser LispVal
parseUnquote = parseQuotes "," (Atom "unquote")

parseUnquoteSplicing :: Parser LispVal -> Parser LispVal
parseUnquoteSplicing = parseQuotes ",@" (Atom "unquote-splicing")

parseMetaVal :: Parser LispVal
parseMetaVal = many1 letter <&> MetaVal

parseMetaAtom :: Parser LispVal
parseMetaAtom = "atom:" *> many1 letter <&> MetaAtom

parseMetaList :: Parser LispVal
parseMetaList = "list:" *> many1 letter <&> MetaList

parseMetaString :: Parser LispVal
parseMetaString = "str:" *> many1 letter <&> MetaString

parseMeta :: Parser LispVal
parseMeta =
  char '@'
    *> (parseMetaAtom <|> parseMetaList <|> parseMetaString <|> parseMetaVal)

parseExpr :: Parser LispVal
parseExpr =
  try parseChar
    <|> try parseString
    <|> try parseNumber
    <|> try (parseList parseExpr)
    <|> try (parseVector parseExpr)
    <|> try (parseQuote parseExpr)
    <|> try (parseQuasiquote parseExpr)
    <|> try (parseUnquote parseExpr)
    <|> try parseAtom

parseExprOrMeta :: Parser LispVal
parseExprOrMeta =
  parseMeta
    <|> try parseChar
    <|> try parseString
    <|> try parseNumber
    <|> try (parseList parseExprOrMeta)
    <|> try (parseVector parseExprOrMeta)
    <|> try (parseQuote parseExprOrMeta)
    <|> try (parseQuasiquote parseExprOrMeta)
    <|> try (parseUnquote parseExprOrMeta)
    <|> try parseAtom

readOrThrow :: Parser a -> Text -> ThrowsError a
readOrThrow parser input = case parseOnly parser input of
  Left  err -> throwError $ Parser err
  Right val -> return val

readExpr :: Text -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: Text -> ThrowsError [LispVal]
readExprList = readOrThrow (sepBy parseExpr skipSpaces)
