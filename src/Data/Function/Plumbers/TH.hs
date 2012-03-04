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
import Data.Maybe (catMaybes)
import Language.Haskell.TH

operatorNames :: Char -> [[String]]
operatorNames o = map (map (o:) . sequence . (`replicate` "^<>&*")) [1..4]

-- | For now this is just a string-yielding function, to be evaluated by the
--   user, to generate the line defining the fixities.
aritiesString :: Char -> String
aritiesString = unlines
              . map (("infixr 9 "++) . concat . intersperse ", ")
              . operatorNames

--TODO: generate better type declarations

implementPlumbers :: Char -> DecsQ
implementPlumbers = return . map implement . concat . operatorNames
 where
  implement :: String -> Dec
  implement (o:vs) = FunD (mkName (o:vs)) [Clause binds (NormalB body) []]
   where
    vars = zip vs $ zip ['a','c'..'y'] ['b','d'..'z']

    binds = map mkVP ["f1", "f2"] ++ map mkBind vars
    mkVP = VarP . mkName
    mkBind ('*', (a, b)) = TupP [mkVP [a], mkVP [b]]
    mkBind (_,   (a, _))   = mkVP [a]

    body = mkOp (mkF "f1" "<&*") (mkF "f2" ">&*")
    mkOp l r | o == '$' = InfixE (Just l) (VarE $ mkName "$") (Just r)
             | o == '*' = TupE [l, r]
    mkVE = VarE . mkName
    mkF f es = foldl1 AppE . (mkVE f:) . catMaybes $ map mkParam vars
     where
      mkParam ('*', (a, b)) = Just $ mkVE [if f == "f1" then a else b]
      mkParam (e,   (a, _))
        | e `elem` es = Just $ mkVE [a]
        | otherwise = Nothing
