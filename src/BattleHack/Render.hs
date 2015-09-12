-- |
-- Module      : BattleHack.Render
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Render where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Monad (forM_)
import Control.Lens
import Data.Complex
-- import qualified Data.Set as S

import qualified Graphics.Rendering.Cairo as Cairo

import BattleHack.Types
import BattleHack.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
data KeyLayout = KeyLeft | KeyRight | KeyBoth | KeyAccidental



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
layout :: RealFloat r => Complex r -> r -> r -> KeyLayout -> [Complex r]
layout (sx:+sy) indent mid KeyRight      = [0:+0,           (sx-indent):+0,   (sx-indent):+mid, sx:+mid, sx:+sy,  0:+sy]                  -- Right indent
layout (sx:+sy) indent mid KeyBoth       = [indent:+0,      (sx-indent):+0,   (sx-indent):+mid, sx:+mid, sx:+sy,  0:+sy,      0:+mid, indent:+mid ]                  -- Double indent
layout (sx:+sy) indent mid KeyLeft       = [indent:+0,      sx:+0,            sx:+sy,           0:+sy,   0:+mid,  indent:+mid] -- Left indent
layout (sx:+sy) indent mid KeyAccidental = [(sx-indent):+0, (sx-indent):+mid, (sx+indent):+mid, (sx+indent):+0]                           -- Accidental
  where
    nw = 0:+0   -- North west
    ne = sx:+0  -- North east
    se = sx:+sy -- South east
    sw = 0:+sy  -- South west

    le = indent:+mid      -- Left indent
    re = (sx-indent):+mid -- Right indent

    lm = 0:+mid  -- Left middle
    rm = sx:+mid -- Right middle



-- |
chordlayout :: [KeyLayout]
chordlayout = [KeyRight, KeyAccidental, KeyBoth, KeyAccidental, KeyLeft, KeyRight, KeyAccidental, KeyBoth, KeyAccidental, KeyBoth, KeyAccidental, KeyLeft]


-- | Horizontal offset for each key in the octave, relative to the very first key
keysteps :: RealFloat r => [r]
keysteps = scanl1 (+) [0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1]



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Vector utilities ------------------------------------------------------------------------------------------------------------------------
-- |
vectorise :: RealFloat f => (f -> f -> a) -> Complex f -> a
vectorise f (x:+y) = f x y


-- General rendering functions -------------------------------------------------------------------------------------------------------------
-- |
polygon :: [Complex Double] -> Cairo.Render ()
polygon (p:oints) = vectorise Cairo.moveTo p >> forM_ (oints++[p]) (vectorise Cairo.lineTo)


-- Piano -----------------------------------------------------------------------------------------------------------------------------------
-- |
key :: (Complex Double, Double, Double) -> Complex Double -> Int -> Cairo.Render ()
key (size, indent, mid) origin i = do
  polygon . map (+origin) $ keylayout
  where
    keylayout     = layout size indent mid (chordlayout !! (i `mod` 12)) :: [Complex Double]


-- |
claviature :: PianoSettings -> Cairo.Render ()
claviature settings = do
  forM_ (zip keysteps [0..11]) $ \(offx, ikey) -> do
    key (size', indent', mid') ((ox+sx*offx):+oy) ikey
    if ikey `elem` naturals
      then Cairo.setSourceRGBA 0.4 (1/7 * fromIntegral (ikey `mod` 7)) 0.75 1.0
      else Cairo.setSourceRGBA 0.0 0.0                                 0.00 1.0
    Cairo.fill

    -- key ((ox+sx*fromIntegral ikey):+oy) ikey
    -- Cairo.setSourceRGBA 0.0 0.0 0.0 1.0
    -- Cairo.stroke
    return ()
  where
    (ox:+oy)        = settings ^. origin
    size'@(sx:+sy)  = settings ^. keysize
    indent'         = sx*settings ^. indent
    mid'            = sy*settings ^. mid


-- Menu ------------------------------------------------------------------------------------------------------------------------------------
