{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Run
  ( run
  )
where

import           Import
import           Data.Version                   ( showVersion )
import           REPL.Console                   ( startREPL )
import           Lang.Quote                     ( lisp )
import           Lang.Primitives                ( primitiveBindings )
import           REPL.Eval                      ( eval )
run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  view (to $ optionsLoad . appOptions) >>= \case
    [] -> do
      logInfo
        ("Scheme48 interpreter, version: " <> fromString (showVersion version))
      startREPL settings
    file -> execFile file


execFile :: FilePath -> RIO App ()
execFile file = do
  env <- primitiveBindings >>= flip bindVars [("args", String file)]
  runIOThrows (show <$> eval env [lisp| (load @str:file) |])
    >>= logError
    .   fromString
    .   extractValue
    .   trapError
