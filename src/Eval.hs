{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Eval where

import           Import
import           Data.List                      ( foldl1 )
import qualified Data.Map                      as Map

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args) ) = apply func $ map eval args
eval (DottedList list val     ) = undefined
eval (Vector vec              ) = undefined
eval val                        = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ Map.lookup func primitives

primitives :: Map String ([LispVal] -> LispVal)
primitives =
  [ ("+"             , numericBinop (+))
  , ("-"             , numericBinop (-))
  , ("*"             , numericBinop (*))
  , ("/"             , numericBinop (/))
  , ("modulo"        , intBinop mod)
  , ("quotient"      , intBinop quot)
  , ("remainder"     , intBinop rem)
  , ("symbol?"       , unaryOp symbolp)
  , ("string?"       , unaryOp stringp)
  , ("number?"       , unaryOp numberp)
  , ("bool?"         , unaryOp boolp)
  , ("list?"         , unaryOp listp)
  , ("symbol->string", unaryOp sym2str)
  , ("string->symbol", unaryOp str2sym)
  ]

numericBinop :: (NumType -> NumType -> NumType) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
 where
  unpackNum (Number n) = n
  unpackNum _          = Integer 0

intBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
intBinop op params = Number . Integer $ foldl1 op $ map unpackInt params
 where
  unpackInt (Number (Integer n)) = n
  unpackInt _                    = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [val] = op val
unaryOp _  _     = undefined

symbolp, numberp, stringp, boolp, listp, sym2str, str2sym :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp (Bool _) = Bool True
boolp _        = Bool False
listp (List _        ) = Bool True
listp (DottedList _ _) = Bool False
listp _                = Bool False
sym2str (Atom sym) = String sym
sym2str _          = String ""
str2sym (String str) = Atom str
str2sym _            = Atom ""
