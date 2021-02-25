{-# LANGUAGE NoImplicitPrelude #-}
module Lang.Primitives.List where

import           Import
import           Control.Monad.Except           ( MonadError(throwError) )


car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [other               ] = throwError $ TypeMismatch "list" other
car other                  = throwError $ NumArgs 1 other

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [other                ] = throwError $ TypeMismatch "list" other
cdr other                   = throwError $ NumArgs 1 other

cons :: [LispVal] -> ThrowsError LispVal
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons other                     = throwError $ NumArgs 2 other
