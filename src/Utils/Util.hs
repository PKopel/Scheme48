{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Util
  ( unop
  , intBinop
  , numBoolBinop
  , strBoolBinop
  , valBoolBinop
  , numMulop
  , boolMulop
  , unpackNum
  , unpackStr
  , unpackChar
  , unpackBool
  )
where

import           RIO
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.List                      ( foldl1 )
import           Utils.Types

type Packer a = a -> LispVal
type Unpacker a = LispVal -> ThrowsError a
type Binop a b = a -> a -> b

unop :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unop op [val] = op val
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

valBoolBinop :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
valBoolBinop = boolBinop return

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
unpackStr other      = throwError $ TypeMismatch "string" other

unpackChar :: Unpacker Char
unpackChar (Char c) = return c
unpackChar other    = throwError $ TypeMismatch "char" other

unpackBool :: Unpacker Bool
unpackBool (Bool b) = return b
unpackBool other    = throwError $ TypeMismatch "boolean" other
