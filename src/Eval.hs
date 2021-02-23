{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Eval where

import           Import
import           Data.List                      ( foldl1 )
import qualified Data.Map                      as Map
import           Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                  ) = return val
eval val@(Number _                  ) = return val
eval val@(Bool   _                  ) = return val
eval (    List   [Atom "quote", val]) = return val
eval (    List   (Atom func : args) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
    $ Map.lookup func primitives

primitives :: Map String ([LispVal] -> ThrowsError LispVal)
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

numericBinop
  :: (NumType -> NumType -> NumType) -> [LispVal] -> ThrowsError LispVal
numericBinop _  []            = throwError $ NumArgs 2 []
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params <&> Number . foldl1 op
 where
  unpackNum :: LispVal -> ThrowsError NumType
  unpackNum (Number n) = return n
  unpackNum other      = throwError $ TypeMismatch "number" other

intBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
intBinop _ [] = throwError $ NumArgs 2 []
intBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
intBinop op params = mapM unpackInt params <&> Number . Integer . foldl1 op
 where
  unpackInt :: LispVal -> ThrowsError Integer
  unpackInt (Number (Integer n)) = return n
  unpackInt other                = throwError $ TypeMismatch "integer" other

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _  []    = throwError $ NumArgs 1 []
unaryOp op [val] = return $ op val
unaryOp _  args  = throwError $ NumArgs 2 args

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
