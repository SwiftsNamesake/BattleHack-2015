-- |
-- Module      : BattleHack.Window
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 18 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Window where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Monad      (forM_, when)
import Control.Concurrent (threadDelay, forkIO)
import Text.Printf
import Data.IORef
import Data.Complex
import Data.StateVar
import qualified Data.Map  as M

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk
import           Graphics.UI.Gtk          (AttrOp(..), on)

-- Internal module imports
import           BattleHack.Types
import qualified BattleHack.Render  as Render
import qualified BattleHack.Events  as Events
import qualified BattleHack.Audio   as Audio



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Return canvas and frame too (?)
-- TODO: Allow for configurations
create :: Vector -> IO (Window, DrawingArea)
create (winx:+winy) = do
  initGUI
  window <- windowNew
  frame  <- frameNew

  set window [windowTitle := "Chordially"]

  canvas <- drawingAreaNew
  containerAdd frame canvas
  set window [ containerChild := frame ]
  windowSetDefaultSize window (round winx) (round winy)
  windowSetIconFromFile window "assets/images/gclef.png"

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  widgetShowAll window

  return (window, canvas)


-- | Register event listeners
-- TODO: Rename (?)
bindevents :: Window -> DrawingArea -> IORef AppState -> IO ()
bindevents window canvas stateref = do
  window `on` deleteEvent        $ Events.ondelete      stateref
  window `on` motionNotifyEvent  $ Events.onmousemotion stateref

  window `on` buttonPressEvent   $ Events.onmousedown stateref
  window `on` buttonReleaseEvent $ Events.onmouseup   stateref

  window `on` keyPressEvent      $ Events.onkeydown stateref
  window `on` keyReleaseEvent    $ Events.onkeyup   stateref

  canvas `on` scrollEvent        $ Events.onwheelscrool stateref
  canvas `on` draw               $ Events.ondraw        stateref

  return ()
