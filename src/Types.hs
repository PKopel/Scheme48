{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           RIO
import           RIO.Process
import           Data.Complex                   ( Complex(..) )
import           Data.Ratio                     ( denominator
                                                , numerator
                                                )

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

data LispVal = Atom String
             | List [LispVal]
             | Vector (Seq LispVal)
             | DottedList [LispVal] LispVal
             | Number NumType
             | Char Char
             | String String
             | Bool Bool

data NumType = Complex (Complex Double)
             | Real Double
             | Rational Rational
             | Integer Integer

instance Show LispVal where
  show (String contents) = "\"" <> contents <> "\""
  show (Atom   name    ) = name
  show (Bool   True    ) = "#t"
  show (Bool   False   ) = "#f"
  show (Number contents) = show contents
  show (Char   char    ) = [char]
  show (List   list    ) = "(" <> unwords (show <$> list) <> ")"
  show (DottedList list val) =
    "(" <> unwords (show <$> list) <> "." <> show val <> ")"
  show (Vector vec) = "#(" <> unwords (toList (show <$> vec)) <> ")"

instance Show NumType where
  show (Integer  num       ) = show num
  show (Real     num       ) = show num
  show (Rational num) = show (numerator num) <> "/" <> show (denominator num)
  show (Complex  (re :+ im)) = show re <> "+" <> show im <> "i"
