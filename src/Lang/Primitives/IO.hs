{-# LANGUAGE NoImplicitPrelude #-}
module Lang.Primitives.IO where

import           Import                  hiding ( hClose )
import           System.IO                      ( readFile
                                                , hClose
                                                , hGetLine
                                                , hPrint
                                                )
import           Lang.Parser                    ( readExpr
                                                , readExprList
                                                )
import           Control.Monad.Except           ( MonadError(..) )

makePort :: IOMode -> [LispVal] -> IOThrowsError App LispVal
makePort mode [String filename] =
  liftIO $ Internal . Port <$> openFile filename mode
makePort _ other = throwError $ NumArgs 1 other

closePort :: [LispVal] -> IOThrowsError App LispVal
closePort [Internal (Port port)] = liftIO $ hClose port $> Bool True
closePort _                      = return $ Bool False

readProc :: [LispVal] -> IOThrowsError App LispVal
readProc [] = readProc [Internal (Port stdin)]
readProc [Internal (Port port)] =
  liftIO (hGetLine port) >>= liftThrows . readExpr . fromString
readProc other = throwError $ NumArgs 1 other

writeProc :: [LispVal] -> IOThrowsError App LispVal
writeProc [obj]                       = writeProc [obj, Internal (Port stdout)]
writeProc [obj, Internal (Port port)] = liftIO $ hPrint port obj $> Bool True
writeProc other                       = throwError $ NumArgs 2 other

readContents :: [LispVal] -> IOThrowsError App LispVal
readContents [String filename] = liftIO $ String <$> readFile filename
readContents other             = throwError $ NumArgs 1 other

load :: String -> IOThrowsError App [LispVal]
load filename =
  liftIO (readFile filename) >>= liftThrows . readExprList . fromString

readAll :: [LispVal] -> IOThrowsError App LispVal
readAll [String filename] = List <$> load filename
readAll other             = throwError $ NumArgs 1 other
