{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Utils.Types.Lisp where

import           RIO
import           Utils.Types.Num
import           Utils.Types.App
import           Control.Monad.Except
import           Data.Data


data LispVal = Atom String
             | List [LispVal]
             | Vector (Seq LispVal)
             | DottedList [LispVal] LispVal
             | Number NumType
             | Char Char
             | String String
             | Bool Bool
             | Internal InType
             | MetaVal String
             | MetaAtom String
             | MetaList String
             | MetaString String
             deriving(Eq, Ord, Data)

instance Show LispVal where
  show (String contents) = "\"" <> contents <> "\""
  show (Atom   name    ) = name
  show (Bool   True    ) = "#t"
  show (Bool   False   ) = "#f"
  show (Number contents) = show contents
  show (Char   char    ) = ['#', '\\', char]
  show (List   list    ) = "(" <> unwords (show <$> list) <> ")"
  show (DottedList list val) =
    "(" <> unwords (show <$> list) <> "." <> show val <> ")"
  show (Vector     vec) = "#(" <> unwords (toList (show <$> vec)) <> ")"
  show (MetaVal    str) = "meta " <> str
  show (MetaAtom   str) = "meta atom " <> str
  show (MetaList   str) = "meta list " <> str
  show (MetaString str) = "meta string " <> str
  show (Internal   val) = show val

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

bindVars :: Env -> [(String, LispVal)] -> RIO a Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
 where
  extendEnv env = (++ env) <$> mapM addBinding bindings
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)

data Fun = Fun  [String]  (Maybe String) [LispVal] Env

data InType = UsrFun Fun
             | PrimFun ([LispVal] -> ThrowsError LispVal)
             | IOFun ([LispVal] -> IOThrowsError App LispVal)
             | Port Handle
             | Mock

instance Eq InType where
  (==) = const (const True)

instance Ord InType where
  compare = const (const EQ)

instance Show InType where
  show (Port _) = "<IO port>"
  show (UsrFun (Fun p v _ _)) =
    "(lambda ("
      <> unwords (show <$> p)
      <> (case v of
           Nothing  -> ""
           Just arg -> " . " <> arg
         )
      <> ") ...)"
  show _ = "<function>"


instance Data InType where
  gunfold _ z _ = z Mock
  toConstr (UsrFun  _) = con "UsrFun"
  toConstr (PrimFun _) = con "PrimFun"
  toConstr (IOFun   _) = con "IOFun"
  toConstr (Port    _) = con "Port"
  toConstr Mock        = con "Mock"
  dataTypeOf _ = tyFunType

con :: String -> Constr
con name = mkConstr tyFunType name [] Prefix
tyFunType :: DataType
tyFunType =
  mkDataType "Module.T" [con "UsrFun", con "PrimFun", con "IOFun", con "Mock"]
