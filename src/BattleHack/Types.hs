-- |
-- Module      : BattleHack.Types
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
module BattleHack.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Sound.OpenAL



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
type Vector = Complex Double


-- |
data KeyLayout = KeyLeft | KeyRight | KeyBoth | KeyAccidental deriving (Show)


-- |
-- data Note = C Pitch | D Pitch | E Pitch | F Pitch | G Pitch | A Pitch | B Pitch deriving (Show)
-- data Pitch = Sharp | Flat | Natural                                             deriving (Show)


-- |
data AppState = AppState { _piano :: PianoSettings, _source :: Source } deriving (Show)


-- |
data PianoSettings = PianoSettings { _origin :: Vector,
                                    _keysize :: Vector,
                                    _indent  :: Double,
                                    _mid     :: Double,
                                    _active  :: Maybe (Int, Bool),
                                    _pressed :: [Bool] } deriving (Show)
