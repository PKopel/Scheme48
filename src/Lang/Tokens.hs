{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Tokens where

import           RIO                            ( Show(..)
                                                , Char
                                                , Double
                                                )
import           Data.Text.Lazy                 ( Text
                                                , unpack
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )


data TokenType = TDot
           | TQuote
           | TQuasiQuote
           | TUnquote
           | TUnquoteSplicing
           | TVecParen
           | TRParen
           | TLParen
           | TTrue
           | TFalse
           | TNum Double
           | TStr Text
           | TChar Char
           | TAtom Text
           | TEOF

instance Show TokenType where
  show TDot             = "."
  show TQuote           = "'"
  show TQuasiQuote      = "`"
  show TUnquote         = ","
  show TUnquoteSplicing = ",@"
  show TRParen          = ")"
  show TLParen          = "("
  show TVecParen        = "#("
  show TTrue            = "#t"
  show TFalse           = "#f"
  show (TNum  x)        = show x
  show (TStr  s)        = "\"" <> unpack s <> "\""
  show (TChar c)        = ['#', '\\', c]
  show (TAtom s)        = unpack s
  show TEOF             = "EOF"
