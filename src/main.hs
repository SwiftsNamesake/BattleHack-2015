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
--        - Serialisation ()



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
  windowSetDefaultSize window 720 480

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  widgetShowAll window

  -- App state
  stateref <- newIORef $ AppState { _piano = PianoSettings { _origin  = 150:+150,
                                                             _keysize = 40:+130,
                                                             _indent  = 0.26,
                                                             _mid     = 0.62,
                                                             _active  = Nothing } }

  -- Register event handlers
  window `on` deleteEvent       $ Events.ondelete      stateref
  window `on` motionNotifyEvent $ Events.onmousemotion stateref
  window `on` buttonPressEvent  $ Events.onmousedown   stateref
  canvas `on` scrollEvent       $ Events.onwheelscrool stateref
  canvas `on` draw              $ Events.ondraw        stateref

  timeoutAdd (Events.onanimate canvas stateref) (1000 `div` fps)

  -- Audio
  Just device <- Audio.setup

  -- Fork off
  forkIO $ do
    audiobuffers <- mapM (\f -> Audio.makebuffer (take (Audio.numSamples 2) $ Audio.sine f)) [440*2**(n/12) | n <- [6, 0, 6, 0]]
    [source]     <- genObjectNames 1

    loopingMode source $= Looping

    queueBuffers source audiobuffers
    play [source]
    threadDelay (5 * 2 * 10^6)

  -- Enter main loop
  mainGUI
  where
    origin@(ox:+oy) = 150:+150
    size@(sx:+sy)   = 40:+130
    indent          = sx*0.26
    mid             = sy*0.62

    fps = 30


-- | Just a little hello world snippet to make sure everything is set up properly.
goodbyeWorld :: IO ()
goodbyeWorld = do
  putStrLn "Hello world!"
  putStrLn "Counting down to launch..."
  forM_ [10,9..0] ((>> threadDelay (10^6)) . print)
  putStrLn "Launching. What have I done!?"
