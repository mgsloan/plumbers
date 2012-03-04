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
-- This module exports 680 operators, all "pointless" combinators for composing
-- functions together with additional plumbing.
--
-----------------------------------------------------------------------------
module Data.Function.Plumbers where

import Data.Function.Plumbers.TH

$(implementPlumbers)

-- http://hackage.haskell.org/trac/ghc/ticket/1541
infixr 9 $^, $<, $>, $&,
         $^^, $^<, $^>, $^&, $<^, $<<, $<>, $<&, $>^, $><, $>>, $>&, $&^, $&<, $&>, $&&,
         $^^^, $^^<, $^^>, $^^&, $^<^, $^<<, $^<>, $^<&, $^>^, $^><, $^>>, $^>&, $^&^, $^&<, $^&>, $^&&, $<^^, $<^<, $<^>, $<^&, $<<^, $<<<, $<<>, $<<&, $<>^, $<><, $<>>, $<>&, $<&^, $<&<, $<&>, $<&&, $>^^, $>^<, $>^>, $>^&, $><^, $><<, $><>, $><&, $>>^, $>><, $>>>, $>>&, $>&^, $>&<, $>&>, $>&&, $&^^, $&^<, $&^>, $&^&, $&<^, $&<<, $&<>, $&<&, $&>^, $&><, $&>>, $&>&, $&&^, $&&<, $&&>, $&&&,
         $^^^^, $^^^<, $^^^>, $^^^&, $^^<^, $^^<<, $^^<>, $^^<&, $^^>^, $^^><, $^^>>, $^^>&, $^^&^, $^^&<, $^^&>, $^^&&, $^<^^, $^<^<, $^<^>, $^<^&, $^<<^, $^<<<, $^<<>, $^<<&, $^<>^, $^<><, $^<>>, $^<>&, $^<&^, $^<&<, $^<&>, $^<&&, $^>^^, $^>^<, $^>^>, $^>^&, $^><^, $^><<, $^><>, $^><&, $^>>^, $^>><, $^>>>, $^>>&, $^>&^, $^>&<, $^>&>, $^>&&, $^&^^, $^&^<, $^&^>, $^&^&, $^&<^, $^&<<, $^&<>, $^&<&, $^&>^, $^&><, $^&>>, $^&>&, $^&&^, $^&&<, $^&&>, $^&&&, $<^^^, $<^^<, $<^^>, $<^^&, $<^<^, $<^<<, $<^<>, $<^<&, $<^>^, $<^><, $<^>>, $<^>&, $<^&^, $<^&<, $<^&>, $<^&&, $<<^^, $<<^<, $<<^>, $<<^&, $<<<^, $<<<<, $<<<>, $<<<&, $<<>^, $<<><, $<<>>, $<<>&, $<<&^, $<<&<, $<<&>, $<<&&, $<>^^, $<>^<, $<>^>, $<>^&, $<><^, $<><<, $<><>, $<><&, $<>>^, $<>><, $<>>>, $<>>&, $<>&^, $<>&<, $<>&>, $<>&&, $<&^^, $<&^<, $<&^>, $<&^&, $<&<^, $<&<<, $<&<>, $<&<&, $<&>^, $<&><, $<&>>, $<&>&, $<&&^, $<&&<, $<&&>, $<&&&, $>^^^, $>^^<, $>^^>, $>^^&, $>^<^, $>^<<, $>^<>, $>^<&, $>^>^, $>^><, $>^>>, $>^>&, $>^&^, $>^&<, $>^&>, $>^&&, $><^^, $><^<, $><^>, $><^&, $><<^, $><<<, $><<>, $><<&, $><>^, $><><, $><>>, $><>&, $><&^, $><&<, $><&>, $><&&, $>>^^, $>>^<, $>>^>, $>>^&, $>><^, $>><<, $>><>, $>><&, $>>>^, $>>><, $>>>>, $>>>&, $>>&^, $>>&<, $>>&>, $>>&&, $>&^^, $>&^<, $>&^>, $>&^&, $>&<^, $>&<<, $>&<>, $>&<&, $>&>^, $>&><, $>&>>, $>&>&, $>&&^, $>&&<, $>&&>, $>&&&, $&^^^, $&^^<, $&^^>, $&^^&, $&^<^, $&^<<, $&^<>, $&^<&, $&^>^, $&^><, $&^>>, $&^>&, $&^&^, $&^&<, $&^&>, $&^&&, $&<^^, $&<^<, $&<^>, $&<^&, $&<<^, $&<<<, $&<<>, $&<<&, $&<>^, $&<><, $&<>>, $&<>&, $&<&^, $&<&<, $&<&>, $&<&&, $&>^^, $&>^<, $&>^>, $&>^&, $&><^, $&><<, $&><>, $&><&, $&>>^, $&>><, $&>>>, $&>>&, $&>&^, $&>&<, $&>&>, $&>&&, $&&^^, $&&^<, $&&^>, $&&^&, $&&<^, $&&<<, $&&<>, $&&<&, $&&>^, $&&><, $&&>>, $&&>&, $&&&^, $&&&<, $&&&>, $&&&&,
         *^, *<, *>, *&,
         *^^, *^<, *^>, *^&, *<^, *<<, *<>, *<&, *>^, *><, *>>, *>&, *&^, *&<, *&>, *&&,
         *^^^, *^^<, *^^>, *^^&, *^<^, *^<<, *^<>, *^<&, *^>^, *^><, *^>>, *^>&, *^&^, *^&<, *^&>, *^&&, *<^^, *<^<, *<^>, *<^&, *<<^, *<<<, *<<>, *<<&, *<>^, *<><, *<>>, *<>&, *<&^, *<&<, *<&>, *<&&, *>^^, *>^<, *>^>, *>^&, *><^, *><<, *><>, *><&, *>>^, *>><, *>>>, *>>&, *>&^, *>&<, *>&>, *>&&, *&^^, *&^<, *&^>, *&^&, *&<^, *&<<, *&<>, *&<&, *&>^, *&><, *&>>, *&>&, *&&^, *&&<, *&&>, *&&&,
         *^^^^, *^^^<, *^^^>, *^^^&, *^^<^, *^^<<, *^^<>, *^^<&, *^^>^, *^^><, *^^>>, *^^>&, *^^&^, *^^&<, *^^&>, *^^&&, *^<^^, *^<^<, *^<^>, *^<^&, *^<<^, *^<<<, *^<<>, *^<<&, *^<>^, *^<><, *^<>>, *^<>&, *^<&^, *^<&<, *^<&>, *^<&&, *^>^^, *^>^<, *^>^>, *^>^&, *^><^, *^><<, *^><>, *^><&, *^>>^, *^>><, *^>>>, *^>>&, *^>&^, *^>&<, *^>&>, *^>&&, *^&^^, *^&^<, *^&^>, *^&^&, *^&<^, *^&<<, *^&<>, *^&<&, *^&>^, *^&><, *^&>>, *^&>&, *^&&^, *^&&<, *^&&>, *^&&&, *<^^^, *<^^<, *<^^>, *<^^&, *<^<^, *<^<<, *<^<>, *<^<&, *<^>^, *<^><, *<^>>, *<^>&, *<^&^, *<^&<, *<^&>, *<^&&, *<<^^, *<<^<, *<<^>, *<<^&, *<<<^, *<<<<, *<<<>, *<<<&, *<<>^, *<<><, *<<>>, *<<>&, *<<&^, *<<&<, *<<&>, *<<&&, *<>^^, *<>^<, *<>^>, *<>^&, *<><^, *<><<, *<><>, *<><&, *<>>^, *<>><, *<>>>, *<>>&, *<>&^, *<>&<, *<>&>, *<>&&, *<&^^, *<&^<, *<&^>, *<&^&, *<&<^, *<&<<, *<&<>, *<&<&, *<&>^, *<&><, *<&>>, *<&>&, *<&&^, *<&&<, *<&&>, *<&&&, *>^^^, *>^^<, *>^^>, *>^^&, *>^<^, *>^<<, *>^<>, *>^<&, *>^>^, *>^><, *>^>>, *>^>&, *>^&^, *>^&<, *>^&>, *>^&&, *><^^, *><^<, *><^>, *><^&, *><<^, *><<<, *><<>, *><<&, *><>^, *><><, *><>>, *><>&, *><&^, *><&<, *><&>, *><&&, *>>^^, *>>^<, *>>^>, *>>^&, *>><^, *>><<, *>><>, *>><&, *>>>^, *>>><, *>>>>, *>>>&, *>>&^, *>>&<, *>>&>, *>>&&, *>&^^, *>&^<, *>&^>, *>&^&, *>&<^, *>&<<, *>&<>, *>&<&, *>&>^, *>&><, *>&>>, *>&>&, *>&&^, *>&&<, *>&&>, *>&&&, *&^^^, *&^^<, *&^^>, *&^^&, *&^<^, *&^<<, *&^<>, *&^<&, *&^>^, *&^><, *&^>>, *&^>&, *&^&^, *&^&<, *&^&>, *&^&&, *&<^^, *&<^<, *&<^>, *&<^&, *&<<^, *&<<<, *&<<>, *&<<&, *&<>^, *&<><, *&<>>, *&<>&, *&<&^, *&<&<, *&<&>, *&<&&, *&>^^, *&>^<, *&>^>, *&>^&, *&><^, *&><<, *&><>, *&><&, *&>>^, *&>><, *&>>>, *&>>&, *&>&^, *&>&<, *&>&>, *&>&&, *&&^^, *&&^<, *&&^>, *&&^&, *&&<^, *&&<<, *&&<>, *&&<&, *&&>^, *&&><, *&&>>, *&&>&, *&&&^, *&&&<, *&&&>, *&&&&