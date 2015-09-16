-- |
-- Module      : Main
-- Description : Entry point module
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : jonatanhsundqvist@gmail.com
-- Stability   : experimental
-- Portability : POSIX

-- TODO | - JSON key bindings and config
--        - Sharing scores and recordings
--        - Buying premade assets, scores, sound fonts, etc. (user content)
--        - Audio!
--        - 3D audio (move listener around)
--        - Serialisation



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
import Control.Monad      (forM_, when)
import Control.Concurrent (threadDelay, forkIO)
import Text.Printf
import Data.IORef
import Data.Complex
import Data.StateVar

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk
import           Graphics.UI.Gtk          (AttrOp(..), on)

import Sound.OpenAL

-- Internal module imports
import           BattleHack.Types
import qualified BattleHack.Render as Render
import qualified BattleHack.Events as Events
import qualified BattleHack.Audio  as Audio



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
  windowSetDefaultSize window (round winx) (round winy)

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  widgetShowAll window

  -- Audio
  Just device <- Audio.setup -- TODO: Return context as well (probably a good idea)
  [source]    <- genObjectNames 1
  -- audiobuffers <- mapM (\f -> Audio.makebuffer (take (Audio.numSamples 2) $ Audio.sine f)) [440*2**(n/12) | n <- [6, 0, 6, 0]]

  -- App state
  stateref <- newIORef $ AppState { _piano = PianoSettings { _origin  = origin',
                                                             _keysize = keysize',
                                                             _indent  = 0.26,
                                                             _mid     = 0.62,
                                                             _active  = Nothing,
                                                             _pressed = replicate 12 False },
                                    _source = source }

  -- Register event handlers
  window `on` deleteEvent        $ Events.ondelete      stateref
  window `on` motionNotifyEvent  $ Events.onmousemotion stateref

  window `on` buttonPressEvent   $ Events.onmousedown stateref
  window `on` buttonReleaseEvent $ Events.onmouseup   stateref

  window `on` keyPressEvent      $ Events.onkeydown stateref
  window `on` keyReleaseEvent    $ Events.onkeyup   stateref

  canvas `on` scrollEvent        $ Events.onwheelscrool stateref
  canvas `on` draw               $ Events.ondraw        stateref

  timeoutAdd (Events.onanimate canvas stateref) (1000 `div` fps)

  -- Enter main loop
  mainGUI
  where
    fps = 30
    origin'@(ox:+oy)  = 20:+20
    keysize'@(sx:+sy) = (4:+13) * 40
    (winx:+winy)      = (sx*7:+sy) + 2*origin'


-- | Just a little hello world snippet to make sure everything is set up properly.
goodbyeWorld :: IO ()
goodbyeWorld = do
  putStrLn "Hello world!"
  putStrLn "Counting down to launch..."
  forM_ [10,9..0] ((>> threadDelay (10^6)) . print)
  putStrLn "Launching. What have I done!?"
