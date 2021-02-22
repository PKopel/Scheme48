{-# LANGUAGE NoImplicitPrelude #-}
module Eval where

import           Import
import           Data.List                      ( foldl1 )

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args) ) = apply func $ map eval args
eval (DottedList list val     ) = undefined
eval (Vector vec              ) = undefined
eval val                        = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("modulo"   , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number . Integer . foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number (Integer n)) = n
unpackNum _                    = 0
