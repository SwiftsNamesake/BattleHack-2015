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
import Control.Applicative
import Data.Complex
import Data.Fixed (mod')
import qualified Data.Set as S

import qualified Graphics.Rendering.Cairo as Cairo

import BattleHack.Types
import BattleHack.Lenses
import BattleHack.Utilities.Vector
import BattleHack.Utilities.General
import BattleHack.Utilities.Math
import qualified BattleHack.Piano as Piano



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
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
      | (piano-->keys) !! ikey            = (0.3, 0.12, 0.22, 1.0)
      | Just ikey == (piano-->active)     = (0.3, 0.12, 0.22, 1.0)
      | mod ikey 12 `elem` Piano.naturals = (0.4, 1/7 * fromIntegral (ikey `mod` 7), 0.75, 1.00)
      | otherwise                         = (0.0, 0.0,                               0.00, 1.00)


-- |
keylabel :: PianoSettings -> Int -> Cairo.Render ()
keylabel piano i = do
  Cairo.setFontSize (if (i `mod` 12) `elem` Piano.naturals then 48 else 22)
  Cairo.selectFontFace "Old English" Cairo.FontSlantNormal Cairo.FontWeightNormal
  Cairo.setSourceRGBA 0.20 0.12 0.08 1.00
  centredText (Piano.keyorigin piano i + o + dotwise (*) sz (0.5:+0.90)) (Piano.notenameFromKeyIndex i)
  where
    (o, sz) = Piano.keybounds piano (Piano.keylayout i)


-- |
claviature :: PianoSettings -> Cairo.Render ()
claviature settings = do
  forM_ (zipWith const [0..] (settings-->keys)) $ \i -> do
    key settings i


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
sinewave :: AppState -> Cairo.Render ()
sinewave appstate = do
  vectorise Cairo.moveTo o
  forM_ [o + (θ:+(48*sin (fromIntegral n/ fromIntegral fps')*sin (θ*0.04))) | θ <- [0,5..600]] (vectorise Cairo.lineTo)
  Cairo.setLineWidth 12
  Cairo.setSourceRGBA 0.62 0.94 0.44 1.0
  Cairo.stroke
  where
    o    = 6.32 * 50:+50
    n    = appstate-->animation.frame
    fps' = appstate-->animation.fps


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
debugHUD :: AppState -> Cairo.Render ()
debugHUD appstate = do
  Cairo.setFontSize 14
  Cairo.selectFontFace "Helvetica" Cairo.FontSlantNormal Cairo.FontWeightNormal
  Cairo.setSourceRGBA 0.05 0.59 0.62 1.00

  let keywidth = appstate-->piano.keysize.real
  when (Piano.insideLeft (appstate-->piano) $ (mouse' & real %~ flip mod' keywidth)) $ do
    centredText (100:+30) "Inside left"

  when (Piano.insideRight (appstate-->piano) $ (mouse' & real %~ flip mod' keywidth)) $ do
    centredText (100:+45) "Inside right"

  maybe pass (\i -> centredText (100:+60) $ Piano.notenameFromKeyIndex i) (appstate-->piano.active)
  -- when (Piano.insideLeft (appstate-->piano) $ (mouse' & real %~ flip mod' keywidth)) $ do
    -- centredText (appstate-->piano.origin + (100:+30)) "Inside left"

  where
    mouse' = appstate-->inputstate.mouse


--------------------------------------------------------------------------------------------------------------------------------------------

-- |
textsize :: String -> Cairo.Render Vector
textsize text = do
  extents <- Cairo.textExtents text
  return $ Cairo.textExtentsWidth extents :+ Cairo.textExtentsHeight extents


-- | Borrowed from Southpaw
-- TODO: General anchor (?)
centredText :: Vector -> String -> Cairo.Render ()
centredText centre text = do
  size <- textsize text
  vectorise Cairo.moveTo $ centre - 0.5*size
  Cairo.showText text


-- Experiments  ----------------------------------------------------------------------------------------------------------------------------

-- |
radial :: AppState -> Cairo.Render ()
radial appstate = Cairo.withRadialPattern a b c d e f $ \ pattern -> do
  Cairo.patternAddColorStopRGBA pattern 0 1 1 1 1
  Cairo.patternAddColorStopRGBA pattern 1 0 0 0 1
  Cairo.setSource pattern
  Cairo.arc mx my 76.8 0 (2*π)
  Cairo.fill

  Cairo.arc mx my 76.8 0 (2*π)
  Cairo.setSourceRGBA 0.0 0.0 0.0 1.0
  Cairo.setLineWidth  12
  Cairo.stroke
  where
    (mx:+my) = appstate-->inputstate.mouse
    [a, b, c, d, e, f] = [115.2, 102.4, 25.6, 102.4, 102.4, 128.0]


-- Menu ------------------------------------------------------------------------------------------------------------------------------------

-- |
overlay :: AppState -> Cairo.Render ()
overlay appstate = do
  -- TODO: Writing animation
  -- Cairo.setOperator Cairo.OperatorDestIn
  Cairo.setFontSize 180
  Cairo.selectFontFace ("Verdana" :: String) Cairo.FontSlantNormal Cairo.FontWeightBold
  Cairo.setSourceRGBA 1.0 1.0 1.0 0.8
  -- centredText ((winx:+winy) * 0.5) "CHORDIAL"
  -- Cairo.setLineWidth 12
  size' <- textsize "CUERDIAL"
  vectorise Cairo.moveTo $ (winsize - size') * 0.5
  Cairo.textPath "CUERDIAL"
  Cairo.clip
  -- Cairo.stroke
  where
    winsize = dotwise (*) (appstate-->piano.keysize) (7:+1) + 2*(appstate-->piano.origin) -- TODO: Find canvas size directly


-- Menu ------------------------------------------------------------------------------------------------------------------------------------
