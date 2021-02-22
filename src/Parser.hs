{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Import                  hiding ( try )
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Complex
import           RIO.Partial                    ( read )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number NumType
             | Char Char
             | String String
             | Bool Bool
             deriving (Show)

data NumType = Complex (Complex Double)
             | Real Double
             | Integer Integer
             deriving (Show)

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
parseChar = "#\\" *> (name <|> letter <|> symbol) <&> Char
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

parseReal :: Parser NumType
parseReal = double <&> Real

parseComplex :: Parser NumType
parseComplex =
  (:+) <$> (double <* oneOf "+-") <*> (double <* char 'i') <&> Complex

parseNumber :: Parser LispVal
parseNumber =
  try parseInteger <|> try parseReal <|> try parseComplex <&> Number

parseList :: Parser LispVal
parseList = do
  char '(' *> skipSpaces
  inits <- sepBy parseExpr skipSpaces
  last  <- skipSpaces *> oneOf ".)"
  case last of
    ')' -> return $ List inits
    _ -> skipSpaces *> parseExpr <* skipSpaces <* char ')' <&> DottedList inits

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseList

readExpr :: Text -> String
readExpr input = case parseOnly (skipMany space >> parseExpr) input of
  Left  err -> "No match: " ++ err
  Right v   -> "Found value " ++ show v
