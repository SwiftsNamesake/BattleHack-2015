-- |
-- Module      : BattleHack.Piano
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
module BattleHack.Piano where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens
import Data.Complex

import BattleHack.Types
import BattleHack.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
naturals :: Integral n => [n]
naturals = [0, 2, 4, 5, 7, 9, 11]


-- |
accidentals :: Integral n => [n]
accidentals = [1, 3, 6, 8, 10]


-- |
chordlayout :: [KeyLayout]
chordlayout = [KeyRight, KeyAccidental, KeyBoth, KeyAccidental, KeyLeft, KeyRight, KeyAccidental, KeyBoth, KeyAccidental, KeyBoth, KeyAccidental, KeyLeft]


-- | Horizontal offset for each key in the octave, relative to the very first key
keysteps :: RealFloat r => [r]
keysteps = scanl1 (+) [0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1]



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
inside :: PianoSettings -> KeyLayout -> Vector -> Bool
inside piano KeyLeft       p = insideBounds piano p && not (insideLeft piano p)
inside piano KeyRight      p = insideBounds piano p && not (insideRight piano p)
inside piano KeyBoth       p = insideBounds piano p && not (insideLeft piano p || insideRight piano p)
inside piano KeyAccidental p = error ""


-- | Is the point within the rectangular bounding box of the key?
insideBounds piano (x:+y) = let (dx:+dy) = piano ^. keysize in between 0 dx x && between 0 dy y


-- | Is the point within the left indent of the key?
insideLeft  piano (x:+y) = let (dx:+dy) = (piano ^. indent):+(piano ^. mid) in between 0 dx y && between 0 dy y


-- | Is the point within the right indent of the key?
insideRight piano p = let shiftX = realPart (piano ^. keysize) + (piano ^. indent) in insideLeft piano $ p - (shiftX:+0)


-- |
between :: Ord n => n -> n -> n -> Bool
between lower upper n = lower <= n && n <= upper
