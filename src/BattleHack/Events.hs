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
import Control.Lens

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import BattleHack.Types
import BattleHack.Lenses
import qualified BattleHack.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
onanimate :: DrawingArea -> IO AppState -> IO Bool
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
  (x, y) <- eventCoordinates
  Cairo.liftIO . putStrLn $ printf "Mouse at x=%.02f, y=%.02f" x y
  return False


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  Cairo.liftIO $ putStrLn "Click!"
  return False


-- |
onwheelscrool :: IORef AppState -> EventM EScroll Bool
onwheelscrool stateref = do
  direction <- eventScrollDirection
  Cairo.liftIO $ modifyIORef stateref (piano.origin %~ (+ deltaX direction))
  Cairo.liftIO $ print direction
  return False
  where
    deltaX ScrollUp   = (-5.0):+0.0
    deltaX ScrollDown = ( 5.0):+0.0
