{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Primitives where

import           Import
import           Util
import           Control.Monad.Except

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [badArg              ] = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  ((==) <$> unpacker arg1 <*> unpacker arg2) `catchError` const (return False)

equalp :: [LispVal] -> ThrowsError LispVal
equalp [DottedList xs x, DottedList ys y] =
  equalp [List $ x : xs, List $ y : ys]
equalp [List arg1, List arg2] =
  return . Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
 where
  eqvPair (x1, x2) = case equalp [x1, x2] of
    Right (Bool val) -> val
    _                -> False
equalp [arg1, arg2] = Bool . or <$> mapM
  (unpackEquals arg1 arg2)
  [ AnyUnpacker unpackNum
  , AnyUnpacker unpackStr
  , AnyUnpacker unpackBool
  , AnyUnpacker return
  ]
equalp badArgList = throwError $ NumArgs 2 badArgList
