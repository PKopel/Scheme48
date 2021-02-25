{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module REPL.Eval where

import           Import
import qualified Data.Map                      as Map
import           Data.List                      ( head
                                                , tail
                                                , last
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Lang.Primitives                ( primitives )
import           Lang.Quote                     ( lisp )

eval :: Env -> LispVal -> IOThrowsError a LispVal
eval _   val@(String _  )                  = return val
eval _   val@(Number _  )                  = return val
eval _   val@(Bool   _  )                  = return val
eval _   val@(Char   _  )                  = return val
eval env (    Atom   var)                  = getVar env var
eval _   [lisp| (quote @val) |]            = return val
eval env [lisp| (set! @atom:var @val) |]   = eval env val >>= setVar env var
eval env [lisp| (define @atom:var @val) |] = eval env val >>= defineVar env var
eval env [lisp| (if @pred @conseq @alt) |] = eval env pred >>= \case
  Bool False -> eval env alt
  Bool True  -> eval env conseq
  _          -> throwError $ TypeMismatch "bool" pred
eval env form@(List (Atom "cond" : clauses))
  | null clauses = throwError
  $ BadSpecialForm "no true clause in cond expression: " form
  | otherwise = case head clauses of
    [lisp| (else @expr) |] -> eval env expr
    [lisp| (@test @expr) |] -> eval env [lisp| (if @test @expr @alt) |]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
  where alt = List (Atom "cond" : tail clauses)
eval env form@(List (Atom "case" : key : clauses))
  | null clauses = throwError
  $ BadSpecialForm "no true clause in case expression: " form
  | otherwise = case head clauses of
    List (Atom "else"   : exprs) -> mapM (eval env) exprs <&> last
    List ((List datums) : exprs) -> do
      result   <- eval env key
      equality <- mapM (\x -> return . Bool $ x == result) datums
      if Bool True `elem` equality
        then mapM (eval env) exprs <&> last
        else eval env $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List (Atom func : args)) =
  mapM (eval env) args >>= liftThrows . apply func
eval _ val@(List _) = return val
eval _ badForm =
  throwError $ BadSpecialForm "unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "unrecognized primitive function args" func)
        ($ args)
    $ Map.lookup func primitives




