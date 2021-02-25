{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Types.App where

import           RIO                     hiding ( toRational )
import           RIO.Process
import           Data.Version                   ( Version )
import           System.Console.Haskeline       ( Settings )

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options,
    appSettings :: !(Settings (RIO App)),
    appVersion :: !Version
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })
