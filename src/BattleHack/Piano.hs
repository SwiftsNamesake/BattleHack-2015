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
import Control.Lens hiding (inside)
import Data.List    (find)
import Data.Complex

import BattleHack.Types
import BattleHack.Lenses
import BattleHack.Utilities.Vector
import BattleHack.Utilities.General



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
naturals :: Integral n => [n]
naturals = [0, 2, 4, 5, 7, 9, 11]


-- |
accidentals :: Integral n => [n]
accidentals = [1, 3, 6, 8, 10]


-- | The indeces of every natural key in order, starting at C0 (index 0)
allnaturals :: [Int]
allnaturals = zipWith (\i key -> div i 7 * 12 + key) [0..] $ cycle naturals


-- | The indeces of every accidental key in order, starting at C#0 (index 1)
allaccidentals :: [Int]
allaccidentals = zipWith (\i key -> div i 5 * 12 + key) [0..] $ cycle accidentals


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
keyorigin piano i = piano-->origin + (sx*shiftx :+ 0)
  where
    (sx:+_)  = piano-->keysize
    shiftx   = 7*octaveFromKeyIndex i + (keysteps !! (i `mod` 12))


-- |
keylayout :: Int -> KeyLayout
keylayout i = chordlayout !! (i `mod` 12)


-- | Is the point inside the bounds of the given key?
--   Note that the function assumes that the key is pinned to the origin.
-- TODO: Bugs ahead (swat them!)
-- TODO: Rename (?)
inside :: PianoSettings -> KeyLayout -> Vector -> Bool
inside piano KeyLeft       p = insideBounds piano p && not (insideLeft piano p)
inside piano KeyRight      p = insideBounds piano p && not (insideRight piano p)
inside piano KeyBoth       p = insideBounds piano p && not (insideLeft piano p || insideRight piano p)
inside piano KeyAccidental p = insideMiddle piano p --  --insideLeft   piano p || insideRight piano p


-- | Is the point within the rectangular bounding box of the key?
insideBounds :: PianoSettings -> Vector -> Bool
insideBounds piano (x:+y) = let (dx:+dy) = piano-->keysize in between 0 dx x && between 0 dy y


-- | Is the point within the left indent of the key?
insideLeft :: PianoSettings -> Vector -> Bool
insideLeft  piano (x:+y) = let (dx:+dy) = dotwise (*) (piano-->indent :+ piano-->mid) (piano-->keysize) in between 0 dx x && between 0 dy y


-- | Is the point within the right indent of the key?
-- TODO: Buggy!
insideRight :: PianoSettings -> Vector -> Bool
insideRight piano p = let shiftx = piano-->keysize.real * (piano-->indent - 1) in insideLeft piano $ p + (shiftx:+0)
-- insideRight piano p = let shiftx = piano-->keysize.real * (1 - piano-->indent) in insideLeft piano $ p - (piano-->keysize.real:+0) + ((piano-->keysize.real*piano-->indent):+0)
-- insideRight piano (x:+y) = between ()


-- |
insideMiddle :: PianoSettings -> Vector -> Bool
insideMiddle piano (x:+y) = let (ox:+oy, dx:+dy) = keybounds piano KeyAccidental in between ox (ox+dx) x && between oy (oy+dy) y


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
keybounds :: PianoSettings -> KeyLayout -> (Vector, Vector)
keybounds piano layout = case layout of
  KeyAccidental -> (piano-->keysize.real' - (indent':+0), (2*indent'):+mid')
  _             -> (0:+0, piano-->keysize)
  where
    (indent':+mid') = dotwise (*) (piano-->keysize) (piano-->indent:+piano-->mid)


-- |
-- TODO: Simplify
-- TODO: Don't hard-code the range
-- TODO: This is a pretty dumb algorithm for finding hovered-over keys
findKeyAt :: Vector -> PianoSettings -> Maybe Int
findKeyAt p piano' = find (insideFromKeyIndex piano' p) (zipWith const [0..] (piano'-->keys))


-- | Is the point inside the key at the given index?
-- TODO: Rename (?)
insideFromKeyIndex :: PianoSettings -> Vector -> Int -> Bool
insideFromKeyIndex piano' p i = inside piano' (keylayout i) (p-keyorigin piano' i)

-- |
pitchFromKeyIndex :: RealFloat r => Int -> r
pitchFromKeyIndex i = let i' = i+4+49 in 440.0*2.0**((fromIntegral i' - 49)/12.0) -- TODO: Make sure this is correct, elaborate on the meaning of the different index conversions
-- pitchFromKeyIndex i = 440*2**((fromIntegral $ i+3 + 12*4-49)/12)


-- | Which octave does the key belong to (the 0th octave starts with C0, the 1st with C1; no overlap between octaves)
octaveFromKeyIndex :: RealFloat r => Int -> r
octaveFromKeyIndex = fromIntegral . (`div` 12)


-- |
-- TODO: Refactor
-- TODO: Use Unicode (?)
notenameFromKeyIndex :: Int -> String
notenameFromKeyIndex i = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"] !! mod i 12
