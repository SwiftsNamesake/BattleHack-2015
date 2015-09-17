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
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}



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
import Data.Maybe
import Data.Functor

import qualified Data.Text as T
import qualified Data.Map  as M

import Control.Lens
import Control.Monad (liftM, forM, void, when)
import Control.Applicative
import Control.Concurrent

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Sound.OpenAL
-- import qualified Sound.ALUT   as Alut

-- import qualified Data.Aeson as JSON
-- import qualified Data.Map   as M

import BattleHack.Types
import BattleHack.Lenses
import BattleHack.Utilities.General
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
  Render.claviature $ state-->piano


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

    -- TODO: Simplify
    setactive mouse appstate    = appstate & piano.active .~ find (hoveredKey (appstate ^. piano) mouse) [0..11]
    hoveredKey piano mouse ikey = Piano.inside piano (Piano.keylayout ikey) (mouse-Piano.keyorigin piano ikey)


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    appstate <- readIORef stateref
    perhaps pass (appstate ^. piano.active) $ \ikey -> do
      loopingMode (_source appstate) $= [OneShot, Looping] !! 1 -- Easy toggling
      void . forkIO $ Audio.note (_source appstate) (Piano.pitchFromKeyIndex ikey) 1.0 -- TODO: Do not hard code duration
  return False


-- |
onmouseup :: IORef AppState -> EventM EButton Bool
onmouseup stateref = do
  -- TODO: Implement note press and release properly
  source <- Cairo.liftIO $ liftM _source $ readIORef stateref
  Cairo.liftIO $ Audio.stopall source
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
  key      <- eventKeyName -- TODO: Use key val?
  bindings <- Cairo.liftIO . liftM (-->bindings) . readIORef $ stateref

  maybe pass Cairo.liftIO (M.lookup (T.unpack key) bindings) --

  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    let mikey = noteIndexFromKey key
    modifyIORef stateref (setactive key)
    perhaps pass mikey $ \ikey -> modifyIORef stateref (piano.keys.ix ikey .~ True)

    perhaps pass (noteIndexFromKey key) $ \iactive -> do
      appstate <- readIORef stateref
      loopingMode (_source appstate) $= [OneShot, Looping] !! 1 -- Easy toggling
      void . forkIO $ Audio.note (_source appstate) (Piano.pitchFromKeyIndex iactive) 1.0 -- TODO: Do not hard code duration

  return False
  where
    setactive key = piano.active .~ noteIndexFromKey key


-- |
onkeyup :: IORef AppState -> EventM EKey Bool
onkeyup stateref = do
  key <- eventKeyName -- TODO: Use key val?
  -- TODO: Implement note press and release properly
  source <- Cairo.liftIO $ liftM _source $ readIORef stateref
  Cairo.liftIO $ do
    let mikey = noteIndexFromKey key
    Audio.stopall source
    modifyIORef stateref (piano.active .~ Nothing)
    perhaps pass mikey $ \ikey -> modifyIORef stateref (piano.keys.ix ikey .~ False)

  return False


-- Utilities -------------------------------------------------------------------------------------------------------------------------------
-- |
noteIndexFromKey :: T.Text -> Maybe Int
noteIndexFromKey k' = (range !!) <$> elemIndex k keys -- TODO: Factor out
  where
    k = head $ T.unpack k'
    keys = "qwertyuiopas" -- TODO: Move out key bindings (eg. to JSON file)
    range = [0..11]       -- TODO: Range should be a setting
