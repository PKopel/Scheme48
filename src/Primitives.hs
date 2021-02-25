{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists #-}
module Primitives where

import           Import
import           Util
import           Primitives.Types
import           Primitives.String
import           Primitives.List
import           Control.Monad.Except           ( MonadError(..) )

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

primitives :: Map String ([LispVal] -> ThrowsError LispVal)
primitives =
  [ ("+"             , numMulop (+))
  , ("-"             , numMulop (-))
  , ("*"             , numMulop (*))
  , ("/"             , numMulop (/))
  , ("modulo"        , intBinop mod)
  , ("quotient"      , intBinop quot)
  , ("remainder"     , intBinop rem)
  , ("symbol?"       , unop (return . symbolp))
  , ("string?"       , unop (return . stringp))
  , ("number?"       , unop (return . numberp))
  , ("bool?"         , unop (return . boolp))
  , ("list?"         , unop (return . listp))
  , ("symbol->string", unop sym2str)
  , ("string->symbol", unop str2sym)
  , ("list->string"  , unop sym2str)
  , ("string->list"  , unop str2sym)
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
  , ("string-ci=?"   , strBoolBinop $ ciOp (==))
  , ("string-ci<?"   , strBoolBinop $ ciOp (<))
  , ("string-ci>?"   , strBoolBinop $ ciOp (>))
  , ("string-ci<=?"  , strBoolBinop $ ciOp (<=))
  , ("string-ci>=?"  , strBoolBinop $ ciOp (>=))
  , ("car"           , car)
  , ("cdr"           , cdr)
  , ("cons"          , cons)
  , ("eq?"           , valBoolBinop (==))
  , ("eqv?"          , valBoolBinop (==))
  , ("equal?"        , equalp)
  , ("make-string"   , makeString)
  , ("string-length" , stringLen)
  , ("string-ref"    , stringRef)
  , ("string-append" , stringAppend)
  ]


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  ((==) <$> unpacker arg1 <*> unpacker arg2) `catchError` const (return False)

equalp :: [LispVal] -> ThrowsError LispVal
equalp [DottedList xs x, DottedList ys y] =
  equalp [List $ x : xs, List $ y : ys]
equalp [List arg1, List arg2] =
  mapM equalPair (zip arg1 arg2) >>= boolMulop (&&) . (:)
    (Bool (length arg1 == length arg2))
  where equalPair (x1, x2) = equalp [x1, x2]
equalp [arg1, arg2] = Bool . or <$> mapM
  (unpackEquals arg1 arg2)
  ([ AnyUnpacker unpackNum
   , AnyUnpacker unpackStr
   , AnyUnpacker unpackBool
   , AnyUnpacker unpackChar
   , AnyUnpacker return
   ] :: [Unpacker]
  )
equalp other = throwError $ NumArgs 2 other
