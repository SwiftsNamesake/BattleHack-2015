-- |
-- Module      : BattleHack.Utilities.Vector
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 14 2015

-- TODO | - Is it silly to use Complex Double for everything because of Cairo (yes it is; fixed) (✓)
--        - Many of these functions should probably be moved to Southpaw

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Utilities.Vector where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex

import BattleHack.Types
import BattleHack.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
dotwise :: (a -> a -> a) -> Complex a -> Complex a -> Complex a
dotwise f (x:+y) (x':+y') = f x x':+f y y'


-- |
dotmap :: (a -> a) -> Complex a -> Complex a
dotmap f (x:+y) = f x:+f y


-- |
toscalar :: (a -> a -> a) -> Complex a -> a
toscalar f (x:+y) = f x y


--  | Converts a 2-tuple to a vector
tovector :: (a, a) -> Complex a
tovector =  uncurry (:+)
