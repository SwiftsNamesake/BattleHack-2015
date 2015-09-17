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
import Control.Monad (forM_, liftM, when)
import Control.Lens
import Data.Complex
import qualified Data.Set as S

import qualified Graphics.Rendering.Cairo as Cairo

import BattleHack.Types
import BattleHack.Lenses
import BattleHack.Utilities.Vector
import qualified BattleHack.Piano as Piano



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------



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
polygon (p:oints) = vectorise Cairo.moveTo p >> forM_ (oints) (vectorise Cairo.lineTo) >> Cairo.closePath


-- Piano -----------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Create utility functions for significant key layout points
key :: PianoSettings -> Int -> Cairo.Render ()
key piano i = do
  polygon . map (+Piano.keyorigin piano i) $ keylayout
  let (r, g, b, a) = colour i in Cairo.setSourceRGBA r g b a
  Cairo.fill

  keylabel piano i
  Cairo.fill
  where
    keylayout = Piano.layout (piano-->keysize) (piano-->indent) (piano-->mid) (Piano.keylayout i) :: [Complex Double]
    colour ikey
      | (piano-->keys) !! ikey        = (0.3, 0.12, 0.22, 1.0)
      | Just ikey == (piano-->active) = (0.3, 0.12, 0.22, 1.0)
      | ikey `elem` Piano.naturals    = (0.4, 1/7 * fromIntegral (ikey `mod` 7), 0.75, 1.00)
      | otherwise                     = (0.0, 0.0,                               0.00, 1.00)


-- |
keylabel :: PianoSettings -> Int -> Cairo.Render ()
keylabel piano i = do
  -- vectorise Cairo.moveTo (Piano.keyorigin piano i + 0.9*(0:+piano-->keysize-->imag)) -- $ (piano-->origin) + dotwise (*) (piano-->keysize) (0.5:+0.8) -- ((piano-->keysize.real)/2:+(piano-->keysize.imag * 0.8))
  let (o, sz) = Piano.keybounds piano i
  -- vectorise Cairo.moveTo (Piano.keyorigin piano i + o + dotwise (*) sz (0.5:+0.95))
  -- vectorise Cairo.moveTo (20:+20)
  Cairo.setFontSize (if i `elem` Piano.naturals then 48 else 22)
  Cairo.setSourceRGBA 0.20 0.12 0.08 1.00
  centredText (Piano.keyorigin piano i + o + dotwise (*) sz (0.5:+0.90)) (Piano.notenameFromKeyIndex i)


-- |
claviature :: PianoSettings -> Cairo.Render ()
claviature settings = do
  forM_ (zipWith const [0..] (settings-->keys)) $ \ikey -> do
    key settings ikey


-- | Borrowed from Southpaw
-- TODO: General anchor (?)
centredText :: Complex Double -> String -> Cairo.Render ()
centredText (cx:+cy) text = do
	extents <- Cairo.textExtents text
	let (w, h) = (Cairo.textExtentsWidth extents, Cairo.textExtentsHeight extents)
	Cairo.moveTo (cx-w/2) (cy+h/2)
	Cairo.showText text


-- Menu ------------------------------------------------------------------------------------------------------------------------------------
