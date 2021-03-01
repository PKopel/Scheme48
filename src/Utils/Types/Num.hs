{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Utils.Types.Num
  ( NumType(..)
  )
where

import           RIO                     hiding ( toRational )
import           Data.Complex                   ( Complex(..) )
import           Data.Ratio                     ( denominator
                                                , numerator
                                                , (%)
                                                )

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

instance Floating NumType where
  pi    = Real pi
  exp   = realOp exp
  log   = realOp log
  sin   = realOp sin
  cos   = realOp cos
  asin  = realOp asin
  acos  = realOp acos
  atan  = realOp atan
  sinh  = realOp sinh
  cosh  = realOp cosh
  asinh = realOp asinh
  acosh = realOp acosh
  atanh = realOp atanh
  sqrt  = realOp sqrt

realOp :: (Double -> Double) -> NumType -> NumType
realOp op (Real a) = Real $ op a
realOp op a        = realOp op (toReal a)

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

