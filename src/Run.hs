{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Import
import           Data.Version                   ( showVersion )
import           REPL.Console
run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  logInfo
    ("Scheme48 interpreter, version: " <> fromString (showVersion version))
  startREPL settings
