-- |
-- Module      : BattleHack.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX

-- Created September 12 2015

-- TODO | -
--        -

-- SPEC | -
--        -



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
import qualified Data.Set as S
import qualified Data.Map as M
import Sound.OpenAL



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
type Number = Double
type Vector = Complex Number
type Sample = Double


-- |
type Command = IO ()
type KeyMap  = M.Map String Command


-- |
data KeyLayout = KeyLeft | KeyRight | KeyBoth | KeyAccidental deriving (Show)


-- |
-- data Note = C Pitch | D Pitch | E Pitch | F Pitch | G Pitch | A Pitch | B Pitch deriving (Show)
-- data Pitch = Sharp | Flat | Natural                                             deriving (Show)


-- |
-- frame number, fps, etc.
data AnimationData = AnimationData { _frame :: Int,
                                     _fps   :: Int }


-- |
data AppState = AppState { _piano      :: PianoSettings,
                           _source     :: Source,
                           _animation  :: AnimationData,
                           _bindings   :: KeyMap,
                           _inputstate :: InputState } -- deriving (Show)


-- |
-- TODO: Use custom key type or text (?)
data InputState =  InputState { _mouse    :: Vector,
                                _keyboard :: S.Set String }


-- |
data PianoSettings = PianoSettings { _origin  :: Vector,
                                     _keysize :: Vector,
                                     _indent  :: Number,
                                     _mid     :: Number,
                                     _active  :: Maybe Int,
                                     _keys    :: [Bool] } deriving (Show)
