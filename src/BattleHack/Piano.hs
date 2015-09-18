-- |
-- Module      : BattleHack.Piano
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX

-- Created September 12 2015

-- TODO | - Split up into several modules
--        -

-- SPEC | -
--        -



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
-- Data
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
keyorigin :: PianoSettings -> Int -> Vector
keyorigin piano ikey = (ox+sx*shiftX):+oy
  where
    (ox:+oy) = piano-->origin
    (sx:+_)  = piano-->keysize
    shiftX   = keysteps !! (ikey `mod` 12)


-- |
keylayout :: Int -> KeyLayout
keylayout i = chordlayout !! (i `mod` 12)


-- |
-- TODO: Bugs ahead (swat them!)
inside :: PianoSettings -> KeyLayout -> Vector -> Bool
inside piano KeyLeft       p = insideBounds piano p && not (insideLeft piano p)
inside piano KeyRight      p = insideBounds piano p && not (insideRight piano p)
inside piano KeyBoth       p = insideBounds piano p && not (insideLeft piano p || insideRight piano p)
inside piano KeyAccidental p = insideRight  piano p || insideLeft piano (p - ((piano-->keysize)-->real:+0))


-- | Is the point within the rectangular bounding box of the key?
insideBounds :: PianoSettings -> Vector -> Bool
insideBounds piano (x:+y) = let (dx:+dy) = piano-->keysize in between 0 dx x && between 0 dy y


-- | Is the point within the left indent of the key?
insideLeft :: PianoSettings -> Vector -> Bool
insideLeft  piano (x:+y) = let (dx:+dy) = piano-->indent :+ piano-->mid in between 0 dx x && between 0 dy y


-- | Is the point within the right indent of the key?
insideRight :: PianoSettings -> Vector -> Bool
insideRight piano p = let shiftX = piano-->keysize.real + piano-->indent in insideLeft piano $ p - (shiftX:+0)


-- |
between :: Ord n => n -> n -> n -> Bool
between lower upper n = lower <= n && n <= upper


-- |
-- TODO: Move to Piano
-- TODO: Rename (?)
layout :: RealFloat r => Complex r -> r -> r -> KeyLayout -> [Complex r]
layout (sx:+sy) indent mid which = case which of
  KeyRight      -> [nw,  nei,  rmi,         rm, se, sw]
  KeyBoth       -> [nwi, nei,  rmi,         rm, se, sw, lm, lmi]
  KeyLeft       -> [nwi, ne,   se,          sw, lm, lmi]
  KeyAccidental -> [nei, rmi,  (sx:+0)+lmi, (sx:+0)+nwi]
  where
    indent' = sx*indent
    mid'    = sy*mid

    nw = 0:+0   -- North west
    ne = sx:+0  -- North east
    se = sx:+sy -- South east
    sw = 0:+sy  -- South west

    nwi = indent':+0       -- North west indented
    nei = (sx-indent'):+0  -- North east indented

    lmi = indent':+mid'      -- Left  middle indent
    rmi = (sx-indent'):+mid' -- Right middle indent

    lm = 0:+mid'  -- Left middle
    rm = sx:+mid' -- Right middle


-- | Rectangular bounds of a key (currently as a topleft, size tuple)
-- TODO: Use Bounding Box type (?)
keybounds :: PianoSettings -> Int -> (Vector, Vector)
keybounds piano i = case keylayout i of
  KeyAccidental -> (piano-->keysize.real' - (indent':+0), (2*indent'):+mid')
  _             -> (0:+0, piano-->keysize)
  where
    indent' = piano-->keysize.real * piano-->indent
    mid'    = piano-->keysize.imag * piano-->mid


-- |
pitchFromKeyIndex :: RealFloat r => Int -> r
pitchFromKeyIndex i = let i' = i+4+49 in 440*2**((fromIntegral i'-49)/12) -- TODO: Make sure this is correct, elaborate on the meaning of the different index conversions
-- pitchFromKeyIndex i = 440*2**((fromIntegral $ i+3 + 12*4-49)/12)


-- |
-- TODO: Refactor
-- TODO: Use Unicode (?)
notenameFromKeyIndex :: Int -> String
notenameFromKeyIndex i = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"] !! mod i 12
