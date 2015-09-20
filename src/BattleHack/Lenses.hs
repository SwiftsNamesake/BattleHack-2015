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
-- import Control.Monad
import Control.Lens
import Data.Functor ((<$>))
import Data.Complex
import qualified Data.Set as S

import BattleHack.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Operators -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Infix declaration
(-->) :: b -> Getting a b a -> a
(-->) = (^.)

infix 8 --> -- Slightly lower precedence than the '.' operator



-- AppState --------------------------------------------------------------------------------------------------------------------------------
-- |
piano :: Lens AppState AppState PianoSettings PianoSettings
piano f s = (\new -> s { _piano=new }) <$> f (_piano s)


bindings :: Lens AppState AppState KeyMap KeyMap
bindings f s = (\new -> s { _bindings=new }) <$> f (_bindings s)


inputstate :: Lens AppState AppState InputState InputState
inputstate f s = (\new -> s { _inputstate=new }) <$> f (_inputstate s)


animation :: Lens AppState AppState AnimationData AnimationData
animation f s = (\new -> s { _animation=new }) <$> f (_animation s)


-- source :: Lens AppState AppState Source Source
-- source f s = (\new -> s { _source=new }) <$> f (_source s)


claviature :: Lens AppState AppState Claviature Claviature
claviature f s = (\new -> s { _claviature=new }) <$> f (_claviature s)


-- AnimationData ---------------------------------------------------------------------------------------------------------------------------
frame :: Lens AnimationData AnimationData Int Int
frame f s = (\new -> s { _frame=new }) <$> f (_frame s)


fps :: Lens AnimationData AnimationData Int Int
fps f s = (\new -> s { _fps=new }) <$> f (_fps s)


-- InputState ------------------------------------------------------------------------------------------------------------------------------
-- |
mouse :: Lens InputState InputState Vector Vector
mouse f s = (\new -> s { _mouse=new }) <$> f (_mouse s)


keyboard :: Lens InputState InputState (S.Set String) (S.Set String)
keyboard f s = (\new -> s { _keyboard=new }) <$> f (_keyboard s)


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
keys :: Lens PianoSettings PianoSettings [Bool] [Bool]
keys f s = (\new -> s { _keys=new }) <$> f (_keys s)


-- Vectors ---------------------------------------------------------------------------------------------------------------------------------
-- | Focuses on the real part (X-component) of a vector.
-- TODO: Real part as vector (eg. x:+y -> x:+0), ditto for imag
real :: Lens (Complex a) (Complex a) a a
real f (x:+y) = (:+y) <$> f x


-- | Focuses on the imaginary part (Y-component) of a vector.
imag :: Lens (Complex a) (Complex a) a a
imag f (x:+y) = (x:+) <$> f y


-- | Like real, except the new type is also a vector.
-- TODO: Better name (?)
real' :: Num n => Lens (Complex n) (Complex n) (Complex n) (Complex n)
real' f (x:+_) = id <$> f (x:+0)


-- | Like imag, except the new type is also a vector.
-- TODO: Better name (?)
imag' :: Num n => Lens (Complex n) (Complex n) (Complex n) (Complex n)
imag' f (_:+y) = id <$> f (0:+y)
