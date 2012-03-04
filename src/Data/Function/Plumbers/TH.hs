-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function.Plumbers.TH
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module is used by Data.Function.Plumbers to generate the operators.
--
-----------------------------------------------------------------------------
module Data.Function.Plumbers.TH where

import Data.List (intersperse)
import Language.Haskell.TH

operatorNames :: [String]
operatorNames = map ('$':) post ++ map ('*':) post
 where
  post = concatMap (sequence . (`replicate` "^<>&")) [1..4]

-- | For now this is just a string-yielding function, to be evaluated by the
--   user, to generate the line defining the fixities.
aritiesString  :: String
aritiesString = "infixr 9 " ++ concat (intersperse ", " operatorNames)

--TODO: generate better type declarations

implementPlumbers :: DecsQ
implementPlumbers = return $ map implement operatorNames
 where
  implement :: String -> Dec
  implement n@(o:vs) = FunD (mkName n) [Clause binds (NormalB body) []]
   where
    binds = map (VarP . mkName) $ ["f1", "f2"] ++ varNames
    appsE = foldl1 AppE . map (VarE . mkName)
    mkOp l r | o == '$' = InfixE (Just l) (VarE $ mkName "$") (Just r)
             | o == '*' = TupE [l, r]
    body = mkOp (appsE $ "f1" : [v | (v, o) <- vars, o `elem` "<&"])
                (appsE $ "f2" : [v | (v, o) <- vars, o `elem` ">&"])

    vars = zip varNames vs
    varNames = take (length vs) $ map (:[]) ['a'..'z']
