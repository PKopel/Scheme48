{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Lang.Quote where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Data.Generics                  ( extQ )
import           Data.Attoparsec.Text           ( skipMany
                                                , space
                                                , parseOnly
                                                )
import           Lang.Parser                    ( parseExpr )
import           Import

runParser :: MonadFail m => String -> m LispVal
runParser str =
  case parseOnly (skipMany space >> parseExpr) (fromString str) of
    Left  err  -> fail err
    Right expr -> return expr


lisp :: QuasiQuoter
lisp = QuasiQuoter { quoteExp  = lispExp
                   , quotePat  = lispPat
                   , quoteType = notHandled "types"
                   , quoteDec  = notHandled "declarations"
                   }
 where
  notHandled things =
    error $ things <> " are not handled by the lisp quasiquoter."

lispExp :: String -> Q Exp
lispExp str = runParser str >>= dataToExpQ (const Nothing `extQ` metaExprExp)

metaExprExp :: LispVal -> Maybe (Q Exp)
metaExprExp (MetaVal v) = Just $ varE (mkName v)
metaExprExp _           = Nothing

lispPat :: String -> Q Pat
lispPat str = runParser str >>= dataToPatQ (const Nothing `extQ` metaExprPat)


metaExprPat :: LispVal -> Maybe (Q Pat)
metaExprPat (MetaVal  v) = Just $ varP (mkName v)
metaExprPat (MetaAtom v) = Just $ conP (mkName "Atom") [varP (mkName v)]
metaExprPat _            = Nothing
