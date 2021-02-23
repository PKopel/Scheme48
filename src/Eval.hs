{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Eval where

import           Import
import           Data.List                      ( foldl1 )
import qualified Data.Map                      as Map
import           Control.Monad.Except

type Packer a = a -> LispVal
type Unpacker a = LispVal -> ThrowsError a
type Binop a b = a -> a -> b

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
  [ ("+"             , numMulop (+))
  , ("-"             , numMulop (-))
  , ("*"             , numMulop (*))
  , ("/"             , numMulop (/))
  , ("modulo"        , intBinop mod)
  , ("quotient"      , intBinop quot)
  , ("remainder"     , intBinop rem)
  , ("symbol?"       , unop symbolp)
  , ("string?"       , unop stringp)
  , ("number?"       , unop numberp)
  , ("bool?"         , unop boolp)
  , ("list?"         , unop listp)
  , ("symbol->string", unop sym2str)
  , ("string->symbol", unop str2sym)
  , ("="             , numBoolBinop (==))
  , ("<"             , numBoolBinop (<))
  , (">"             , numBoolBinop (>))
  , ("/="            , numBoolBinop (/=))
  , (">="            , numBoolBinop (>=))
  , ("<="            , numBoolBinop (<=))
  , ("&&"            , boolMulop (&&))
  , ("||"            , boolMulop (||))
  , ("string=?"      , strBoolBinop (==))
  , ("string<?"      , strBoolBinop (<))
  , ("string>?"      , strBoolBinop (>))
  , ("string<=?"     , strBoolBinop (<=))
  , ("string>=?"     , strBoolBinop (>=))
  ]


unop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unop op [val] = return $ op val
unop _  args  = throwError $ NumArgs 1 args

binop :: Unpacker a -> Packer b -> Binop a b -> [LispVal] -> ThrowsError LispVal
binop unpack pack op [l, r] = op <$> unpack l <*> unpack r <&> pack
binop _      _    _  args   = throwError $ NumArgs 2 args

intBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
intBinop = binop unpackInt (Number . Integer)

boolBinop :: Unpacker a -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpack = binop unpack Bool

numBoolBinop :: (NumType -> NumType -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

mulop :: Unpacker a -> Packer a -> Binop a a -> [LispVal] -> ThrowsError LispVal
mulop unpack pack op args | length args < 2 = throwError $ NumArgs 2 args
                          | otherwise = mapM unpack args <&> pack . foldl1 op

numMulop :: (NumType -> NumType -> NumType) -> [LispVal] -> ThrowsError LispVal
numMulop = mulop unpackNum Number

boolMulop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMulop = mulop unpackBool Bool

unpackInt :: Unpacker Integer
unpackInt (Number (Integer n)) = return n
unpackInt other                = throwError $ TypeMismatch "integer" other

unpackNum :: Unpacker NumType
unpackNum (Number n) = return n
unpackNum other      = throwError $ TypeMismatch "number" other

unpackStr :: Unpacker String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: Unpacker Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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
