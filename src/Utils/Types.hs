{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Utils.Types where

import           RIO                     hiding ( toRational )
import           RIO.Process
import           Data.Version                   ( Version )
import           System.Console.Haskeline       ( Settings )
import           Control.Monad.Except           ( MonadError(catchError) )
import           Data.Complex                   ( Complex(..) )
import           Data.Ratio                     ( denominator
                                                , numerator
                                                , (%)
                                                )

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

data LispVal = Atom String
             | List [LispVal]
             | Vector (Seq LispVal)
             | DottedList [LispVal] LispVal
             | Number NumType
             | Char Char
             | String String
             | Bool Bool
             | MetaLispVal String
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
  show (Vector      vec) = "#(" <> unwords (toList (show <$> vec)) <> ")"
  show (MetaLispVal str) = "meta " <> str

data NumType = Complex (Complex Double)
             | Real Double
             | Rational Rational
             | Integer Integer
             deriving(Eq, Ord, Data)

instance Ord a => Ord (Complex a) where
  compare (ar :+ ai) (br :+ bi) = case compare ar br of
    EQ    -> compare ai bi
    other -> other


instance Show NumType where
  show (Integer  num       ) = show num
  show (Rational num) = show (numerator num) <> "/" <> show (denominator num)
  show (Real     num       ) = show num
  show (Complex  (re :+ im)) = show re <> "+" <> show im <> "i"


toComplex :: NumType -> NumType
toComplex (    Integer  num) = Complex $ fromInteger num :+ 0
toComplex num@(Rational _  ) = toComplex $ toReal num
toComplex (    Real     num) = Complex $ num :+ 0
toComplex num@(Complex  _  ) = num

toReal :: NumType -> NumType
toReal (Integer num) = Real $ fromInteger num
toReal (Rational num) =
  Real $ fromInteger (numerator num) / fromInteger (denominator num)
toReal num@(Real _) = num
toReal _            = undefined

toRational :: NumType -> NumType
toRational (    Integer  num) = Rational $ fromInteger num
toRational num@(Rational _  ) = num
toRational _                  = undefined

instance Num NumType where
  (Integer a) + (Integer b) = Integer $ a + b
  a           + b           = numTypeOp (+) a b
  (Integer a) * (Integer b) = Integer $ a * b
  a           * b           = numTypeOp (*) a b
  abs         = numTypeFun abs
  signum      = numTypeFun signum
  negate      = numTypeFun negate
  fromInteger = Integer

instance Fractional NumType where
  (Integer a) / (Integer b) = Rational $ a % b
  a           / b           = numTypeOp (/) a b
  fromRational = Rational

numTypeOp
  :: (forall  a . (Num a, Fractional a) => a -> a -> a)
  -> NumType
  -> NumType
  -> NumType
numTypeOp op (  Real     a) (Real     b)   = Real $ op a b
numTypeOp op (  Rational a) (Rational b)   = Rational $ op a b
numTypeOp op (  Complex  a) (Complex  b)   = Complex $ op a b
numTypeOp op a@(Complex  _) b              = numTypeOp op a (toComplex b)
numTypeOp op a              b@(Complex _)  = numTypeOp op (toComplex a) b
numTypeOp op a@(Real _)     b              = numTypeOp op a (toReal b)
numTypeOp op a              b@(Real _)     = numTypeOp op (toReal a) b
numTypeOp op a@(Rational _) b              = numTypeOp op a (toRational b)
numTypeOp op a              b@(Rational _) = numTypeOp op (toRational a) b
numTypeOp _  _              _              = undefined

numTypeFun :: (forall  a . Num a => a -> a) -> NumType -> NumType
numTypeFun fun (Integer  a) = Integer $ fun a
numTypeFun fun (Real     a) = Real $ fun a
numTypeFun fun (Rational a) = Rational $ fun a
numTypeFun fun (Complex  a) = Complex $ fun a

