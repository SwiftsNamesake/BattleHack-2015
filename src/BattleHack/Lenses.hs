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



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens
-- import Control.Monad
import BattleHack.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- AppState lenses -------------------------------------------------------------------------------------------------------------------------
-- |
piano :: Lens AppState AppState PianoSettings PianoSettings
piano f s = (\new -> s { _piano=new }) `fmap` f (_piano s)


-- PianoSettings lenses --------------------------------------------------------------------------------------------------------------------
-- |
origin :: Lens PianoSettings PianoSettings Vector Vector
origin f s = (\new -> s { _origin=new }) `fmap` f (_origin s)


-- |
keysize :: Lens PianoSettings PianoSettings Vector Vector
keysize f s = (\new -> s { _keysize=new }) `fmap` f (_keysize s)


-- |
indent :: Lens PianoSettings PianoSettings Double Double
indent f s = (\new -> s { _indent=new }) `fmap` f (_indent s)


-- |
mid :: Lens PianoSettings PianoSettings Double Double
mid f s = (\new -> s { _mid=new }) `fmap` f (_mid s)
