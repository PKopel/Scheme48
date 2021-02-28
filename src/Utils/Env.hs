{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Env where

import           RIO
import           Utils.Types.Lisp
import           Control.Monad.Except           ( MonadError(throwError) )

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

bindVars :: Env -> [(String, LispVal)] -> RIO a Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
 where
  extendEnv env = (++ env) <$> mapM addBinding bindings
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)
