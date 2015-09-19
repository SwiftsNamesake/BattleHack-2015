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

-- import qualified Data.Aeson as JSON

import qualified Data.Text as T
import qualified Data.Map  as M
import qualified Data.Set  as S

import Control.Lens
import Control.Monad (liftM, forM, void, when, unless, liftM)
import Control.Applicative
import Control.Concurrent

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Sound.OpenAL

import BattleHack.Types
import BattleHack.Lenses
import BattleHack.Utilities.General
import BattleHack.Utilities.Vector
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
  modifyIORef stateref (animation.frame %~ (+1)) -- Increment frame count
  return True


-- |
ondraw :: IORef AppState -> Cairo.Render ()
ondraw stateref = do
  appstate <- Cairo.liftIO $ readIORef stateref
  Render.claviature $ appstate-->piano --
  Render.debugHUD   $ appstate         --
  -- Render.sinewave   $ appstate         -- Just for shits and giggles


-- |
ondelete :: IORef AppState -> EventM EAny Bool
ondelete stateref = do
  Cairo.liftIO $ putStrLn "Good bye"
  Cairo.liftIO $ mainQuit >> return False


-- |
-- TODO: Capture mouse drag
onmousemotion :: IORef AppState -> EventM EMotion Bool
onmousemotion stateref = do
  mouse' <- liftM tovector eventCoordinates
  Cairo.liftIO $ do
    modifyIORef stateref (setactive mouse')
    modifyIORef stateref (inputstate.mouse .~ mouse')
  return False
  where
    -- TODO: Simplify
    -- TODO: Don't hard-code the range
    setactive mouse' appstate    = appstate & piano.active .~ find (hoveredKey (appstate-->piano) mouse') (zipWith const [0..] (appstate-->piano.keys))
    hoveredKey piano mouse' ikey = Piano.inside piano (Piano.keylayout ikey) (mouse'-Piano.keyorigin piano ikey)


-- |
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    appstate <- readIORef stateref
    perhaps pass (appstate-->piano.active) $ \ikey -> do
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
-- TODO: Helpers for updating state, checking repeats
onkeydown :: IORef AppState -> EventM EKey Bool
onkeydown stateref = do
  key      <- eventKeyName -- TODO: Use key val?
  bindings <- Cairo.liftIO . liftM (-->bindings) . readIORef $ stateref

  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    appstate <- readIORef stateref

    -- Unless the key is already being pressed
    unless (S.member (T.unpack key) $ appstate-->inputstate.keyboard) $ do
      modifyIORef stateref (inputstate.keyboard %~ S.insert (T.unpack key)) -- Mark key as pressed
      print key
      maybe pass id (M.lookup (T.unpack key) bindings) -- Invoke the command bound to this key (if any)
      let mikey = noteIndexFromKey key
      -- modifyIORef stateref (setactive key)
      perhaps pass mikey $ \ikey -> modifyIORef stateref (piano.keys.ix ikey .~ True)

      perhaps pass (noteIndexFromKey key) $ \iactive -> do
        appstate <- readIORef stateref
        loopingMode (_source appstate) $= [OneShot, Looping] !! 1 -- Easy toggling
        void . forkIO $ Audio.note (_source appstate) (Piano.pitchFromKeyIndex iactive) 1.0 -- TODO: Do not hard code duration

  return False
  -- where
  --   setactive key = piano.active .~ noteIndexFromKey key


-- |
onkeyup :: IORef AppState -> EventM EKey Bool
onkeyup stateref = do
  key <- eventKeyName -- TODO: Use key val?
  -- TODO: Implement note press and release properly
  source <- Cairo.liftIO $ liftM _source $ readIORef stateref
  Cairo.liftIO $ do
    modifyIORef stateref (inputstate.keyboard %~ S.delete (T.unpack key)) -- Mark key as pressed
    let mikey = noteIndexFromKey key
    Audio.stopall source
    -- modifyIORef stateref (piano.active .~ Nothing)
    perhaps pass mikey $ \ikey -> modifyIORef stateref (piano.keys.ix ikey .~ False)

  return False


-- Utilities -------------------------------------------------------------------------------------------------------------------------------
-- |
noteIndexFromKey :: T.Text -> Maybe Int
noteIndexFromKey k' = M.lookup k mapping -- TODO: Factor out
  where
    k = head $ T.unpack k'
    mapping = M.fromList $ zip "asdfghjklöä" Piano.allnaturals ++ zip "wetyupå"  Piano.allaccidentals
    -- keys = "asdfghjklöä" ++ "we tyu på" -- TODO: Move out key bindings (eg. to JSON file)
    -- range = map ((0*12)+) Piano.naturals      -- TODO: Range should be a setting
