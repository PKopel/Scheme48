{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module REPL.Eval where

import           Import
import           Data.List                      ( head
                                                , tail
                                                , last
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Lang.Quote                     ( lisp )
import           Lang.Primitives.IO             ( load )

eval :: Env -> LispVal -> IOThrowsError App LispVal
eval _   val@(String _)      = return val
eval _   val@(Number _)      = return val
eval _   val@(Bool   _)      = return val
eval _   val@(Char   _)      = return val
eval env [lisp| @atom:var |] = getVar env var
eval env [lisp| (quasiquote @list:vals) |] =
  evalQuasiAcc env List (1, 0) vals []
eval env [lisp| (quasiquote @vec:vals) |] =
  evalQuasiAcc env toVector (1, 0) (toList vals) []
eval _ [lisp| (Quasi @val) |] = return val
eval _ [lisp| (quote @val) |] = return val
eval env [lisp| (load @str:filename) |] =
  load filename >>= fmap last . mapM (eval env)
eval env [lisp| (set! @atom:var @val) |]   = eval env val >>= setVar env var
eval env [lisp| (define @atom:var @val) |] = eval env val >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
  = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env [lisp| (if @pred @conseq @alt) |] = eval env pred >>= \case
  Bool False -> eval env alt
  Bool True  -> eval env conseq
  _          -> throwError $ TypeMismatch "bool" pred
eval env form@(List (Atom "cond" : clauses))
  | null clauses = throwError
  $ BadSpecialForm "no true clause in cond expression: " form
  | otherwise = case head clauses of
    [lisp| (else @expr) |] -> eval env expr
    [lisp| (@test @expr) |] -> eval env [lisp| (if @test @expr @list:alt) |]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
  where alt = Atom "cond" : tail clauses
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
eval env (List (Atom "apply" : func : args)) = do
  fun     <- eval env func
  argVals <- mapM (eval env) args >>= \case
    [List argVals] -> return argVals
    other          -> return other
  apply fun argVals
eval env (List (func : args)) = do
  fun     <- eval env func
  argVals <- mapM (eval env) args
  apply fun argVals
eval _ badForm =
  throwError $ BadSpecialForm "unrecognized special form" badForm

evalQuasiAcc
  :: Env
  -> ([LispVal] -> LispVal)
  -> (Int, Int)
  -> [LispVal]
  -> [LispVal]
  -> IOThrowsError App LispVal
evalQuasiAcc _ constr _ [] acc = return . constr $ reverse acc
evalQuasiAcc env constr lvls@(qLevel, uLevel) ([lisp| (@atom:key @val) |] : vals) acc
  | qLevel == uLevel + 1 && key `elem` ["unquote", "unquote-splicing"]
  = do
    let cat = case key of
          "unquote"          -> Right . (: acc)
          "unquote-splicing" -> fmap ((<> acc) . reverse) . unpackList
          _                  -> undefined
    eval env val >>= liftThrows . cat >>= evalQuasiAcc env constr lvls vals
  | key `elem` ["unquote", "unquote-splicing", "quasiquote"]
  = do
    (constr', val', lvls') <- case [lisp| (@atom:key @val) |] of
      [lisp| (unquote @list:value) |] ->
        return (List, value, (qLevel, uLevel + 1))
      [lisp| (unquote-splicing @list:value) |] ->
        return (List, value, (qLevel, uLevel + 1))
      [lisp| (quasiquote @list:value) |] ->
        return (List, value, (qLevel + 1, uLevel))
      [lisp| (unquote @vec:value) |] ->
        return (toVector, toList value, (qLevel, uLevel + 1))
      [lisp| (quasiquote @vec:value) |] ->
        return (toVector, toList value, (qLevel + 1, uLevel))
      other -> throwError $ BadSpecialForm "unrecognized special form" other
    res <- evalQuasiAcc env constr' lvls' val' []
    evalQuasiAcc env constr lvls vals ([lisp| (@atom:key @res) |] : acc)
evalQuasiAcc env constr lvls (val : vals) acc =
  evalQuasiAcc env constr lvls vals (val : acc)

apply :: LispVal -> [LispVal] -> IOThrowsError App LispVal
apply (Internal (IOFun   func)) args = func args
apply (Internal (PrimFun func)) args = liftThrows $ func args
apply (Internal (UsrFun (Fun params varargs body closure))) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else
      lift (bindVars closure $ zip params args)
      >>= bindVarArgs varargs
      >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  evalBody env = last <$> mapM (eval env) body
  bindVarArgs arg env = case arg of
    Just argName -> lift $ bindVars env [(argName, List remainingArgs)]
    Nothing      -> return env
apply val _ = throwError $ NotFunction "unrecognized function" $ show val

makeFunc
  :: (Monad m, Show a) => Maybe String -> Env -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return . Internal . UsrFun $ Fun (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError App LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs
  :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError App LispVal
makeVarArgs = makeFunc . Just . show
