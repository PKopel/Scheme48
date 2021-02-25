{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Types.Env where

import           RIO
import           Utils.Types.Lisp
import           Control.Monad.Except


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser String
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (UnboundVar     message varname) = message <> ": " <> varname
  show (BadSpecialForm message form   ) = message <> ": " <> show form
  show (NotFunction    message func   ) = message <> ": " <> show func
  show (NumArgs expected found) =
    "Expected " <> show expected <> " args; found values " <> unwords
      (show <$> found)
  show (TypeMismatch expected found) =
    "Invalid type: expected " <> expected <> ", found " <> show found
  show (Parser  parseErr) = "Parse error at " <> show parseErr
  show (Default err     ) = err

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _           = undefined

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError a = ExceptT LispError (RIO a)

nullEnv :: RIO a Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError b a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError a String -> RIO a (ThrowsError String)
runIOThrows action = runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

withEnv :: (IORef LispVal -> IO a) -> Env -> String -> IOThrowsError b a
withEnv ioRefOp envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "unbound variable" var)
        (liftIO . ioRefOp)
        (lookup var env)

getVar :: Env -> String -> IOThrowsError a LispVal
getVar = withEnv readIORef

setVar :: Env -> String -> LispVal -> IOThrowsError a LispVal
setVar envRef var value = withEnv (`writeIORef` value) envRef var $> value

defineVar :: Env -> String -> LispVal -> IOThrowsError a LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env      <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
 where
  extendEnv env = (++ env) <$> mapM addBinding bindings
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)
