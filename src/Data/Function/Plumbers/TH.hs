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
module Data.Function.Plumbers.TH
  ( implementPlumbers, implementPlumber
  , PlumberSpec(..), baseSpec, compositionSpec, productSpec
  , operatorNames, aritiesString
  ) where

import Control.Applicative ((<$>))
import Data.Bits (testBit)
import Data.List (intersperse)
import Language.Haskell.TH

-- | Specifies all of the information needed to implement a plumber.
data PlumberSpec = PlumberSpec
 { plumberOpE        :: Exp -> Exp -> Exp     -- ^ Operation to apply to function results
 , plumberLeftTypes  :: [String] -> [String]  -- ^ Transformation on left argument list types
 , plumberRightTypes :: [String] -> [String]  -- ^ Transformation on right argument list types
 , plumberResultType :: Type                  -- ^ Results type
 , plumberMinArity   :: Int                   -- ^ Maximum arity to generate
 , plumberMaxArity   :: Int                   -- ^ Maximum arity to generate
 , plumberPrefix     :: String                -- ^ Prefix to use for operator
 }

baseSpec, compositionSpec, productSpec :: PlumberSpec

baseSpec = PlumberSpec
  { plumberOpE        = undefined
  , plumberLeftTypes  = id
  , plumberRightTypes = id
  , plumberResultType = mkVT "r1"
  , plumberMinArity   = 1
  , plumberMaxArity   = 3
  , plumberPrefix     = undefined
  }

compositionSpec = baseSpec
  { plumberOpE        = (\l r -> InfixE (Just l) (mkVE "$") (Just r))
  , plumberLeftTypes  = (++ ["r2"])
  , plumberPrefix     = "$"
  }

productSpec = baseSpec
  { plumberOpE        = (\l r -> TupE [l, r]) 
  , plumberResultType = tuplesT [mkVT "r1", mkVT "r2"]
  , plumberPrefix     = "*"
  } 

{-
bindSpec = baseSpec
  { plumberOpE        = (\l r -> InfixE (Just l) (mkVE "=<<") (Just r)) 

  , plumberPrefix     = "<="
  } )
 -}

-- | All of the operator names that the given PlumberSpec would implement.
operatorNames :: PlumberSpec -> [[String]]
operatorNames s = map (map (plumberPrefix s ++) . sequence . (`replicate` "^<>&*"))
                      [plumberMinArity s .. plumberMaxArity s]

-- | For now this is just a string-yielding function, to be evaluated by the
--   user, to generate the line defining the fixities.  This code should be
--   pasted below the TH invocation of implementPlumbers
aritiesString :: PlumberSpec -> String
aritiesString = unlines
              . map (("infixr 9 "++) . concat . intersperse ", ")
              . operatorNames

-- | Implements all of the plumbers specified by the given @PlumberSpec@.
implementPlumbers :: PlumberSpec -> DecsQ
implementPlumbers spec = concat <$> mapM (implementPlumber spec) (concat $ operatorNames spec)

-- | Implement only the specific plumber requested.
implementPlumber :: PlumberSpec -> String -> DecsQ
implementPlumber spec name@(_:vs)
  = return
    [ SigD (mkName name) typ
    , FunD (mkName name) [Clause binds (NormalB body) []]
    ]
 where
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
      $ [ arrowsT . map mkVT $ (plumberLeftTypes  spec) args1 ++ ["r1"]
        , arrowsT . map mkVT $ (plumberRightTypes spec) args2 ++ ["r2"] ]
      ++ map mkTyp params
      ++ [plumberResultType spec]
  --TODO: make/find helpers library?
  mkTyp (Right (a, b)) = tuplesT [mkVT a, mkVT b]
  mkTyp (Left a) = mkVT a

  binds = map mkVP ["f1", "f2"] ++ map mkBind directives
  mkBind (0, _) = WildP
  mkBind (_, Left a) = mkVP a
  mkBind (_, Right (a, b)) = TupP [mkVP a, mkVP b]

  body = plumberOpE spec (mkF "f1" args1) (mkF "f2" args2)
  mkF n = foldl1 AppE . (mkVE n:) . map mkVE

-- * Template Haskell Utilities

-- TODO: consider whether tricks like http://hpaste.org/54367 could aid this

-- TODO: domain-generic construction methods for TH.  E.g. have a "var" function
-- that is polymorphic on return type.

appsT, arrowsT, tuplesT :: [Type] -> Type
appsT = foldl1 AppT
arrowsT = foldr1 (\x y -> appsT [ArrowT, x, y])
tuplesT xs = appsT $ [TupleT $ length xs] ++ xs

mkVE :: String -> Exp
mkVE = VarE . mkName
mkVP :: String -> Pat
mkVP = VarP . mkName
mkVT :: String -> Type
mkVT = VarT . mkName
mkVB :: String -> TyVarBndr
mkVB = PlainTV . mkName