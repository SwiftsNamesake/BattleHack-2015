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
-- import qualified Data.Set as S

import qualified Graphics.Rendering.Cairo as Cairo

import BattleHack.Types
import BattleHack.Lenses
import qualified BattleHack.Piano as Piano



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Move to Piano
layout :: RealFloat r => Complex r -> r -> r -> KeyLayout -> [Complex r]
layout (sx:+sy) indent mid which = case which of
  KeyRight ->      [nw,  nei,  rmi,         rm, se, sw]
  KeyBoth  ->      [nwi, nei,  rmi,         rm, se, sw, lm, lmi]
  KeyLeft  ->      [nwi, ne,   se,          sw, lm, lmi]
  KeyAccidental -> [nei, rmi,  (sx:+0)+lmi, (sx:+0)+nwi]
  where
    nw = 0:+0   -- North west
    ne = sx:+0  -- North east
    se = sx:+sy -- South east
    sw = 0:+sy  -- South west

    nwi = indent:+0       -- North west indented
    nei = (sx-indent):+0  -- North east indented

    lmi = indent:+mid      -- Left  middle indent
    rmi = (sx-indent):+mid -- Right middle indent

    lm = 0:+mid  -- Left middle
    rm = sx:+mid -- Right middle



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
    keylayout     = layout size indent mid (Piano.keyLayout i) :: [Complex Double]


-- |
claviature :: PianoSettings -> Cairo.Render ()
claviature settings = do
  forM_ [0..11] $ \ikey -> do
    key (size', indent', mid') (Piano.keyOrigin settings ikey) ikey
    let (r, g, b, a) = colour ikey in Cairo.setSourceRGBA r g b a
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
    colour ikey
      | (settings ^. pressed) !! ikey               = (0.3, 0.12, 0.22, 1.0)
      | Just ikey == liftM fst (settings ^. active) = (0.3, 0.12, 0.22, 1.0)
      | ikey `elem` Piano.naturals                  = (0.4, 1/7 * fromIntegral (ikey `mod` 7), 0.75, 1.00)
      | otherwise                                   = (0.0, 0.0,                               0.00, 1.00)


-- Menu ------------------------------------------------------------------------------------------------------------------------------------
