{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function.Plumbers.TH
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module exports 2 * (5 + 5^2 + 5^3) = 310 operators, all `pointless'
-- combinators for composing functions together with additional plumbing.
--
-----------------------------------------------------------------------------
module Data.Function.Plumbers where

import Prelude hiding ((**))

import Data.Function.Plumbers.TH

$(implementPlumbers compositionSpec)

-- http://hackage.haskell.org/trac/ghc/ticket/1541 ...
infixr 9 $^, $<, $>, $&, $*
infixr 9 $^^, $^<, $^>, $^&, $^*, $<^, $<<, $<>, $<&, $<*, $>^, $><, $>>, $>&, $>*, $&^, $&<, $&>, $&&, $&*, $*^, $*<, $*>, $*&, $**
infixr 9 $^^^, $^^<, $^^>, $^^&, $^^*, $^<^, $^<<, $^<>, $^<&, $^<*, $^>^, $^><, $^>>, $^>&, $^>*, $^&^, $^&<, $^&>, $^&&, $^&*, $^*^, $^*<, $^*>, $^*&, $^**, $<^^, $<^<, $<^>, $<^&, $<^*, $<<^, $<<<, $<<>, $<<&, $<<*, $<>^, $<><, $<>>, $<>&, $<>*, $<&^, $<&<, $<&>, $<&&, $<&*, $<*^, $<*<, $<*>, $<*&, $<**, $>^^, $>^<, $>^>, $>^&, $>^*, $><^, $><<, $><>, $><&, $><*, $>>^, $>><, $>>>, $>>&, $>>*, $>&^, $>&<, $>&>, $>&&, $>&*, $>*^, $>*<, $>*>, $>*&, $>**, $&^^, $&^<, $&^>, $&^&, $&^*, $&<^, $&<<, $&<>, $&<&, $&<*, $&>^, $&><, $&>>, $&>&, $&>*, $&&^, $&&<, $&&>, $&&&, $&&*, $&*^, $&*<, $&*>, $&*&, $&**, $*^^, $*^<, $*^>, $*^&, $*^*, $*<^, $*<<, $*<>, $*<&, $*<*, $*>^, $*><, $*>>, $*>&, $*>*, $*&^, $*&<, $*&>, $*&&, $*&*, $**^, $**<, $**>, $**&, $***

$(implementPlumbers productSpec)

infixr 9 *^, *<, *>, *&, **
infixr 9 *^^, *^<, *^>, *^&, *^*, *<^, *<<, *<>, *<&, *<*, *>^, *><, *>>, *>&, *>*, *&^, *&<, *&>, *&&, *&*, **^, **<, **>, **&, ***
infixr 9 *^^^, *^^<, *^^>, *^^&, *^^*, *^<^, *^<<, *^<>, *^<&, *^<*, *^>^, *^><, *^>>, *^>&, *^>*, *^&^, *^&<, *^&>, *^&&, *^&*, *^*^, *^*<, *^*>, *^*&, *^**, *<^^, *<^<, *<^>, *<^&, *<^*, *<<^, *<<<, *<<>, *<<&, *<<*, *<>^, *<><, *<>>, *<>&, *<>*, *<&^, *<&<, *<&>, *<&&, *<&*, *<*^, *<*<, *<*>, *<*&, *<**, *>^^, *>^<, *>^>, *>^&, *>^*, *><^, *><<, *><>, *><&, *><*, *>>^, *>><, *>>>, *>>&, *>>*, *>&^, *>&<, *>&>, *>&&, *>&*, *>*^, *>*<, *>*>, *>*&, *>**, *&^^, *&^<, *&^>, *&^&, *&^*, *&<^, *&<<, *&<>, *&<&, *&<*, *&>^, *&><, *&>>, *&>&, *&>*, *&&^, *&&<, *&&>, *&&&, *&&*, *&*^, *&*<, *&*>, *&*&, *&**, **^^, **^<, **^>, **^&, **^*, **<^, **<<, **<>, **<&, **<*, **>^, **><, **>>, **>&, **>*, **&^, **&<, **&>, **&&, **&*, ***^, ***<, ***>, ***&, ****

-- $(implementPlumbers rbindSpec)