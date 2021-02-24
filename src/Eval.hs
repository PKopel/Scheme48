{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Eval where

import           Import
import qualified Data.Map                      as Map
import           Data.List                      ( head
                                                , tail
                                                , last
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Primitives
import           Util
import           Quote                          ( lisp )

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                   = return val
eval val@(Number _)                   = return val
eval val@(Bool   _)                   = return val
eval [lisp| (quote ?val)|]            = return val
eval [lisp| (if ?pred ?conseq ?alt)|] = eval pred >>= \case
  Bool False -> eval alt
  Bool True  -> eval conseq
  _          -> throwError $ TypeMismatch "bool" pred
eval form@(List (Atom "cond" : clauses))
  | null clauses = throwError
  $ BadSpecialForm "no true clause in cond expression: " form
  | otherwise = case head clauses of
    [lisp| (else ?expr)|] -> eval expr
    [lisp| (?test ?expr)|] -> eval [lisp| (if ?test ?expr ?alt)|]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
  where alt = List (Atom "cond" : tail clauses)
eval form@(List (Atom "case" : key : clauses))
  | null clauses = throwError
  $ BadSpecialForm "no true clause in case expression: " form
  | otherwise = case head clauses of
    List (Atom "else"   : exprs) -> mapM eval exprs <&> last
    List ((List datums) : exprs) -> do
      result   <- eval key
      equality <- mapM (\x -> return . Bool $ x == result) datums
      if Bool True `elem` equality
        then mapM eval exprs <&> last
        else eval $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "unrecognized primitive function args" func)
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



