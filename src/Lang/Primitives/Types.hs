{-# LANGUAGE NoImplicitPrelude #-}
module Lang.Primitives.Types where

import           Import
import           Control.Monad.Except           ( MonadError(..) )


symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
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

str2sym, sym2str, str2list, list2str :: LispVal -> ThrowsError LispVal
str2sym (String str) = return $ Atom str
str2sym other        = throwError $ TypeMismatch "string" other
sym2str (Atom sym) = return $ String sym
sym2str other      = throwError $ TypeMismatch "atom" other
str2list (String str) = return . List $ map Char str
str2list other        = throwError $ TypeMismatch "string" other
list2str (List list) | all isChar list = String <$> mapM unpackChar list
 where
  isChar (Char _) = True
  isChar _        = False
list2str other = throwError $ TypeMismatch "list of char" other
