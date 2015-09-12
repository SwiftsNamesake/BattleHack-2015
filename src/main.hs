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

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk
import           Graphics.UI.Gtk          (AttrOp(..), on)

import qualified BattleHack.Render as Render



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  Gtk.initGUI
  window <- Gtk.windowNew
  frame  <- Gtk.frameNew

  -- Gtk.windowTitle := "Chordially"
  Gtk.set window [Gtk.windowTitle := "Chordially"]

  canvas <- Gtk.drawingAreaNew
  containerAdd frame canvas
  Gtk.set window [ Gtk.containerChild := frame ]
  Gtk.windowSetDefaultSize window 720 480

  widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask
  Gtk.widgetShowAll window

  --
  window `on` Gtk.deleteEvent $ (Cairo.liftIO $ Gtk.mainQuit >> return False)
  canvas `on` Gtk.draw        $ (Render.claviature)

  Gtk.mainGUI


-- |
goodbyeWorld :: IO ()
goodbyeWorld = do
  putStrLn "Hello world!"
  putStrLn "Counting down to launch..."
  forM_ [9,8..0] ((>> threadDelay (10^6)) . print)
  putStrLn "Launching. What have I done!?"
