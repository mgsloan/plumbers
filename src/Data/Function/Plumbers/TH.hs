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

import Data.Bits (testBit)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Language.Haskell.TH

operatorNames :: Int -> String -> [[String]]
operatorNames n o = map (map (o++) . sequence . (`replicate` "^<>&*")) [1..n]

-- | For now this is just a string-yielding function, to be evaluated by the
--   user, to generate the line defining the fixities.
aritiesString :: Int -> String -> String
aritiesString n = unlines
                . map (("infixr 9 "++) . concat . intersperse ", ")
                . operatorNames n


data PlumberSpec = PlumberSpec
 { plumberOpE        :: Exp -> Exp -> Exp     -- ^ Operation to apply to function results
 , plumberLeftTypes  :: [String] -> [String]  -- ^ Transformation on left argument list types
 , plumberRightTypes :: [String] -> [String]  -- ^ Transformation on right argument list types
 , plumberResultType :: Type                  -- ^ Results type
 , plumberArity      :: Int                   -- ^ Maximum arity to generate
 , plumberPrefix     :: String                -- ^ Prefix to use for operator
 }

implementPlumbers :: PlumberSpec -> DecsQ
implementPlumbers (PlumberSpec opE transT1 transT2 rT num prefix)
  = return . concatMap implement . concat $ operatorNames num prefix
 where
  implement :: String -> [Dec]
  implement n@(o:vs) = [ SigD name typ
                       , FunD name [Clause binds (NormalB body) []]
                       ]
   where
    name = mkName n

    directives :: [(Int, Either String (String, String))]
    directives = rec vs (map (:[]) ['a'..'z'])
     where
      rec [] _ = []
      rec ('^':xs) (y  :ys) = (0, Left y)       : rec xs ys
      rec ('<':xs) (y  :ys) = (1, Left y)       : rec xs ys
      rec ('>':xs) (y  :ys) = (2, Left y)       : rec xs ys
      rec ('&':xs) (y  :ys) = (3, Left y)       : rec xs ys
      rec ('*':xs) (y:z:ys) = (3, Right (y, z)) : rec xs ys

    params = map snd directives
    names = concatMap (either (:[]) (\(y, z) -> [y, z])) params
    args1 = [either id fst x | (i, x) <- directives, testBit i 0]
    args2 = [either id snd x | (i, x) <- directives, testBit i 1]

    typ = ForallT (map mkVB $ names ++ ["r1", "r2"]) [] . arrowsT
        $ [ arrowsT . map mkVT $ transT1 args1 ++ ["r1"]
          , arrowsT . map mkVT $ transT2 args2 ++ ["r2"] ]
        ++ map mkTyp params
        ++ [rT]
    --TODO: make/find helpers library?
    mkTyp (Right (a, b)) = tuplesT [mkVT a, mkVT b]
    mkTyp (Left a) = mkVT a

    binds = map mkVP ["f1", "f2"] ++ map mkBind params
    mkBind (Left a) = mkVP a
    mkBind (Right (a, b)) = TupP [mkVP a, mkVP b]

    body = opE (mkF "f1" args1)  (mkF "f2" args2)
    mkF n = foldl1 AppE . (mkVE n:) . map mkVE

appsT = foldl1 AppT
arrowsT = foldr1 (\x y -> appsT [ArrowT, x, y])
tuplesT xs = appsT $ [TupleT $ length xs] ++ xs

mkVE = VarE . mkName
mkVP = VarP . mkName
mkVT = VarT . mkName
mkVB = PlainTV . mkName

-- TODO: domain-generic construction methods for TH.  E.g. have a "var" function
-- that is polymorphic on return type.
