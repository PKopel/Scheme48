{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  )
where

import           Import
import           Run
import           RIO.Process
import           Options.Applicative.Simple
import qualified Paths_Scheme48
import           System.Console.Haskeline

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_Scheme48.version)
    "Scheme48 - simple Lisp interpreter"
    "start the interpreter"
    (   Options
    <$> switch (long "verbose" <> short 'v' <> help "verbose output")
    <*> strOption
          (long "load" <> short 'l' <> metavar "FILE" <> value "" <> help
            "execute program from FILE"
          )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  let settings = Settings { complete       = noCompletion
                          , historyFile    = Nothing
                          , autoAddHistory = True
                          }
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appOptions        = options
                  , appSettings       = settings
                  , appVersion        = Paths_Scheme48.version
                  }
    in  runRIO app run
