{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Eval where

import           Import
import qualified Data.Map                      as Map
import           Control.Monad.Except           ( MonadError(throwError) )
import           Primitives
import           Util

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                             ) = return val
eval val@(Number _                             ) = return val
eval val@(Bool   _                             ) = return val
eval (    List   [Atom "quote", val]           ) = return val
eval (    List   [Atom "if", pred, conseq, alt]) = eval pred >>= \case
  Bool False -> eval alt
  Bool True  -> eval conseq
  other      -> throwError $ TypeMismatch "bool" other
eval (List (Atom func : args)) = mapM eval args >>= apply func
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
  , ("car"           , car)
  , ("cdr"           , cdr)
  , ("cons"          , cons)
  , ("eq?"           , valBoolBinop (==))
  , ("eqv?"          , valBoolBinop (==))
  , ("equal?"        , equalp)
  ]



