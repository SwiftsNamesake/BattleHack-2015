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
import BattleHack.Utilities.Math
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
  -- Render.overlay    $ appstate         --
  Render.claviature $ appstate-->piano --
  Render.debugHUD   $ appstate         --
  -- Render.radial     $ appstate         --
  -- Render.sinewave   $ appstate         -- Just for shits and giggles


-- |
ondelete :: IORef AppState -> EventM EAny Bool
ondelete stateref = Cairo.liftIO $ do
  putStrLn "Good bye"
  mainQuit
  return False


-- |
-- TODO: Capture mouse drag
onmousemotion :: IORef AppState -> EventM EMotion Bool
onmousemotion stateref = do
  mouse' <- liftM tovector eventCoordinates
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    modifyIORef stateref (piano.active     .~ Piano.findKeyAt mouse' (appstate-->piano))
    modifyIORef stateref (inputstate.mouse .~ mouse')
  return False


-- |
-- Do yourself a favour and pretend you never saw this mess
onmousedown :: IORef AppState -> EventM EButton Bool
onmousedown stateref = do
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    perhaps pass (appstate-->piano.active) $ \i -> do
      -- loopingMode (_source appstate) $= [OneShot, Looping] !! 1 -- Easy toggling
      -- void . forkIO $ Audio.note (_source appstate) (Piano.pitchFromKeyIndex ikey) 1.0 -- TODO: Do not hard code duration
      -- void $ Audio.playnote (appstate-->claviature) i
      modifyIORef stateref (piano.keys.ix i .~ True)
  return False


-- |
onmouseup :: IORef AppState -> EventM EButton Bool
onmouseup stateref = do
  -- TODO: Implement note press and release properly
  -- source <- Cairo.liftIO $ liftM _source $ readIORef stateref
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    perhaps pass (appstate-->piano.active) $ \i -> do
      modifyIORef stateref (piano.keys.ix i .~ False)
      -- return expressionvoid $ Audio.stopnote (appstate-->claviature) i
  return False


-- |
onwheelscrool :: IORef AppState -> EventM EScroll Bool
onwheelscrool stateref = do
  direction <- eventScrollDirection
  Cairo.liftIO $ do
    modifyIORef stateref (piano.origin %~ panX direction)
  return False
  where
    panX ScrollUp   = (+ ((-5.0):+0.0))
    panX ScrollDown = (+ (( 5.0):+0.0))


-- |
-- TODO: Helpers for updating state, checking repeats
onkeydown :: IORef AppState -> EventM EKey Bool
onkeydown stateref = do
  key <- T.unpack <$> eventKeyName -- TODO: Use key val?

  Cairo.liftIO $ do
    -- Do yourself a favour and pretend you never saw this mess
    bindings <- liftM (-->bindings) . readIORef $ stateref
    appstate <- readIORef stateref

    -- Unless the key is already being pressed
    unless (S.member key $ appstate-->inputstate.keyboard) $ do
      modifyIORef stateref (inputstate.keyboard %~ S.insert key) -- Mark key as pressed
      fromMaybe pass (M.lookup key bindings)                     -- Invoke the command bound to this key (if any)

      perhaps pass (noteIndexFromKey key) $ \i -> do
        appstate <- readIORef stateref
        modifyIORef stateref (piano.keys.ix i .~ True)
        -- void $ Audio.playnote (appstate-->claviature) i

  return False


-- |
-- TODO: Implement note press and release properly
onkeyup :: IORef AppState -> EventM EKey Bool
onkeyup stateref = do
  key <- T.unpack <$> eventKeyName -- TODO: Use key val?
  Cairo.liftIO $ do
    appstate <- readIORef stateref
    modifyIORef stateref (inputstate.keyboard %~ S.delete key) -- Mark key as pressed
    perhaps pass (noteIndexFromKey key) $ \i -> do
      -- Audio.stopnote (appstate-->claviature) i
      modifyIORef stateref (piano.keys.ix i .~ False)

  return False


-- Utilities -------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Don't use 'head' (decapitations are dangerous)
-- TODO: Move out key bindings (eg. to JSON file)
-- TODO: Range should be a setting
noteIndexFromKey :: String -> Maybe Int
noteIndexFromKey key = M.lookup (head key) mapping -- TODO: Factor out
  where
    mapping = M.fromList $ zip "asdfghjklöä" Piano.allnaturals ++ zip "wetyupå"  Piano.allaccidentals
