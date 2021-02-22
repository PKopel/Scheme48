{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Eval where

import           Import
import           Data.List                      ( foldl1 )
import qualified Data.Map                      as Map

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args) ) = apply func $ map eval args
eval (DottedList list val     ) = undefined
eval (Vector vec              ) = undefined
eval val                        = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ Map.lookup func primitives

primitives :: Map String ([LispVal] -> LispVal)
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop (/))
  , ("modulo"   , intBinop mod)
  , ("quotient" , intBinop quot)
  , ("remainder", intBinop rem)
  ]

numericBinop :: (NumType -> NumType -> NumType) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
 where
  unpackNum (Number n) = n
  unpackNum _          = Integer 0

intBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
intBinop op params = Number . Integer $ foldl1 op $ map unpackInt params
 where
  unpackInt (Number (Integer n)) = n
  unpackInt _                    = 0
