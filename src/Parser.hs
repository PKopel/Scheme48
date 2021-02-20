{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import           Import
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           RIO.Partial                    ( read )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

oneOf :: [Char] -> Parser Char
oneOf list = satisfy (`elem` list)

noneOf :: [Char] -> Parser Char
noneOf list = satisfy (`notElem` list)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escaped :: Parser Char 
escaped = char '\\' *> oneOf "\\\"\'nrt"

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many (noneOf "\\\"" <|> escaped) <* char '"')

parseAtom :: Parser LispVal
parseAtom = do
  firstLetter <- letter <|> symbol
  rest        <- many (letter <|> digit <|> symbol)
  let atom = firstLetter : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: Text -> String
readExpr input = case parse (skipSpace >> parseExpr) input of
  Fail _ _ err -> "No match: " ++ err
  Partial _    -> "Partial result"
  Done _ _     -> "Found value"
