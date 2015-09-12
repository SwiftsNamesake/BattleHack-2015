-- |
-- Module      : Main
-- Description : Entry point module
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : sample@email.com
-- Stability   : experimental
-- Portability : POSIX



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Monad      (forM_)
import Control.Concurrent (threadDelay)
import Text.Printf
import Data.IORef
import Data.Complex

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk
import           Graphics.UI.Gtk          (AttrOp(..), on)

-- Internal module imports
import           BattleHack.Types
import qualified BattleHack.Render as Render
import qualified BattleHack.Events as Events



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  initGUI
  window <- windowNew
  frame  <- frameNew

  set window [windowTitle := "Chordially"]

  canvas <- drawingAreaNew
  containerAdd frame canvas
  set window [ containerChild := frame ]
  windowSetDefaultSize window 720 480

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  widgetShowAll window

  -- App state
  stateref <- newIORef $ AppState { _piano = PianoSettings { _origin=150:+150, _keysize=40:+130, _indent=0.26, _mid=0.62 } }

  -- Register event handlers
  window `on` deleteEvent       $ Events.ondelete stateref
  window `on` motionNotifyEvent $ Events.onmousemotion stateref
  window `on` buttonPressEvent  $ Events.onmousedown stateref
  canvas `on` scrollEvent       $ Events.onwheelscrool stateref
  canvas `on` draw              $ Events.ondraw stateref

  -- canvas `on` onaimate
  

  mainGUI
  where
    origin@(ox:+oy) = 150:+150
    size@(sx:+sy)   = 40:+130
    indent          = sx*0.26
    mid             = sy*0.62


-- | Just a little hello world snippet to make sure everything is set up properly.
goodbyeWorld :: IO ()
goodbyeWorld = do
  putStrLn "Hello world!"
  putStrLn "Counting down to launch..."
  forM_ [9,8..0] ((>> threadDelay (10^6)) . print)
  putStrLn "Launching. What have I done!?"
