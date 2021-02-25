{-# LANGUAGE NoImplicitPrelude #-}
module Lang.Primitives.String where

import           Import
import           Data.List                      ( (!!) )
import           Data.Char                      ( toLower )
import           Control.Monad.Except           ( MonadError(throwError) )

ciOp :: (String -> String -> a) -> String -> String -> a
ciOp op = \a b -> op (ci a) (ci b) where ci = map toLower

makeString :: [LispVal] -> ThrowsError LispVal
makeString [len@(Number _)] = makeString [len, Char ' ']
makeString [Number (Integer len), Char c] =
  return . String $ replicate (fromIntegral len) c
makeString [Number _, other] = throwError $ TypeMismatch "char" other
makeString [other]           = throwError $ TypeMismatch "number" other
makeString other             = throwError $ NumArgs 2 other

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [String s] = Right $ Number $ fromIntegral $ length s
stringLen [other   ] = throwError $ TypeMismatch "string" other
stringLen other      = throwError $ NumArgs 1 other

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number (Integer k)]
  | length s < k' + 1 = throwError $ Default "Out of bound error"
  | otherwise         = return . String $ [s !! k']
  where k' = fromIntegral k
stringRef [String _, other] = throwError $ TypeMismatch "number" other
stringRef [other   , _    ] = throwError $ TypeMismatch "string" other
stringRef other             = throwError $ NumArgs 2 other

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend list
  | length list < 2   = throwError $ NumArgs 2 list
  | all isString list = foldM catStr (String "") list
  | otherwise         = throwError $ TypeMismatch "list of strings" (List list)
 where
  catStr str1 str2 = (<>) <$> unpackStr str1 <*> unpackStr str2 <&> String
  isString (String _) = True
  isString _          = False

