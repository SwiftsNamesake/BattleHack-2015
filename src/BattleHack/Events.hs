-- |
-- Module      : BattleHack.Events
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
{-# LANGUAGE TupleSections #-}


--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Events where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Text.Printf
import Data.IORef
import Data.Complex
import Data.List
import Control.Lens
import Control.Monad (liftM, forM)
import Control.Concurrent

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Sound.OpenAL

import BattleHack.Types
import BattleHack.Lenses
import qualified BattleHack.Piano  as Piano
import qualified BattleHack.Audio  as Audio
import qualified BattleHack.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
onanimate :: DrawingArea -> IORef AppState -> IO Bool
onanimate canvas stateref = do
  widgetQueueDraw canvas
  return True


-- |
ondraw :: IORef AppState -> Cairo.Render ()
ondraw stateref = do
  state <- Cairo.liftIO $ readIORef stateref
  Render.claviature (state ^. piano)


-- |
ondelete :: IORef AppState -> EventM EAny Bool
ondelete stateref = do
  Cairo.liftIO $ putStrLn "Good bye"
  Cairo.liftIO $ mainQuit >> return False


-- |
onmousemotion :: IORef AppState -> EventM EMotion Bool
onmousemotion stateref = do
  mouse <- liftM tovector eventCoordinates
  Cairo.liftIO $ modifyIORef stateref (setactive mouse)
  return False
  where
    tovector :: (Double, Double) -> Complex Double
    tovector =  uncurry (:+)

    setactive mouse appstate    = appstate & piano.active .~ ( liftM (, False) $ find (hoveredKey (appstate ^. piano) mouse) [0..11])
    hoveredKey piano mouse ikey = Piano.inside piano (Piano.keyLayout ikey) (mouse-Piano.keyOrigin piano ikey)


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    appstate <- readIORef stateref
    flip (maybe (return ())) (appstate ^. piano.active) $ \(ikey, _) -> do
      loopingMode (_source appstate) $= [OneShot, Looping] !! 1 -- Easy toggling
      forkIO $ Audio.note (_source appstate) (440*2**(fromIntegral ikey/12)) 1.0 -- TODO: Do not hard code duration
      return ()
    putStrLn "Click!"
  return False


-- |
onmouseup :: IORef AppState -> EventM EButton Bool
onmouseup stateref = do
  -- TODO: Implement note press and release properly
  source <- Cairo.liftIO $ liftM _source $ readIORef stateref
  stop [source]
  unqueueBuffers source 1
  return False


-- |
onwheelscrool :: IORef AppState -> EventM EScroll Bool
onwheelscrool stateref = do
  direction <- eventScrollDirection
  Cairo.liftIO $ modifyIORef stateref (piano.origin %~ panX direction)
  Cairo.liftIO $ print direction
  return False
  where
    panX ScrollUp   = (+ ((-5.0):+0.0))
    panX ScrollDown = (+ (( 5.0):+0.0))


-- |
onkeydown :: IORef AppState -> EventM EKey Bool
onkeydown stateref = do
  key <- eventKeyName -- TODO: Use key val?

  return False
