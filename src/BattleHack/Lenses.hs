-- |
-- Module      : BattleHack.Lenses
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX

-- Created September 12 2015



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens
import Data.Functor ((<$>))
import Data.Complex
-- import Control.Monad

import BattleHack.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Operators -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Infix declaration
(-->) :: b -> Getting a b a -> a
(-->) = (^.)


-- AppState --------------------------------------------------------------------------------------------------------------------------------
-- |
piano :: Lens AppState AppState PianoSettings PianoSettings
piano f s = (\new -> s { _piano=new }) <$> f (_piano s)


-- PianoSettings ---------------------------------------------------------------------------------------------------------------------------
-- |
origin :: Lens PianoSettings PianoSettings Vector Vector
origin f s = (\new -> s { _origin=new }) <$> f (_origin s)


-- |
keysize :: Lens PianoSettings PianoSettings Vector Vector
keysize f s = (\new -> s { _keysize=new }) <$> f (_keysize s)


-- |
indent :: Lens PianoSettings PianoSettings Number Number
indent f s = (\new -> s { _indent=new }) <$> f (_indent s)


-- |
mid :: Lens PianoSettings PianoSettings Number Number
mid f s = (\new -> s { _mid=new }) <$> f (_mid s)


-- |
active :: Lens PianoSettings PianoSettings (Maybe Int) (Maybe Int)
active f s = (\new -> s { _active=new }) <$> f (_active s)


-- |
pressed :: Lens PianoSettings PianoSettings [Bool] [Bool]
pressed f s = (\new -> s { _pressed=new }) <$> f (_pressed s)


-- Vectors ---------------------------------------------------------------------------------------------------------------------------------
-- |
real :: Lens (Complex a) (Complex a) a a
real f (x:+y) = (:+y) <$> f x


-- |
imag :: Lens (Complex a) (Complex a) a a
imag f (x:+y) = (x:+) <$> f y
