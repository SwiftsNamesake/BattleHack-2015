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
--        - IO Lenses (?)
--        - Better way of dealing with state, sharing IO resources
--        - Restrict access to AppState, 'fenced off' IORefs



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
import Control.Monad      (forM_, when, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Text.Printf
import Data.IORef
import Data.Complex
import Data.StateVar
import Data.Functor
import qualified Data.Set  as S
import qualified Data.Map  as M

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          as Gtk
import           Graphics.UI.Gtk          (AttrOp(..), on)

import Sound.OpenAL
-- import qualified Sound.ALUT   as Alut

-- Internal module imports
import           BattleHack.Types
import           BattleHack.Lenses
import qualified BattleHack.Render  as Render
import qualified BattleHack.Events  as Events
import qualified BattleHack.Audio   as Audio
import qualified BattleHack.Window  as Window



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  -- Create and configure window
  (window, canvas) <- Window.create winsize

  -- Audio
  -- TODO: Utility function for accessing range indeces
  -- TODO: Don't hard-code range
  Just (context, device) <- Audio.setup -- TODO: Return context as well (probably a good idea) (✓)
  [source] <- genObjectNames 1
  -- claviature'            <- Audio.makebuffersFromIndeces (zipWith const [0..] $ replicate 24 False)

  -- App state
  stateref <- newIORef (initalstate 24 origin' keysize') -- claviature')

  --
  mnotes <- newMVar . (-->piano.keys) <$> readIORef stateref

  forkIO $ Audio.stream (1.0/30.0) source mnotes

  forkIO $ forever $ do
    putMVar mnotes . (-->piano.keys) <$> readIORef stateref
    threadDelay $ (1.0/30.0) * 10^6

  -- Register event handlers
  Window.bindevents window canvas stateref

  -- Animation
  timeoutAdd (Events.onanimate canvas stateref) (1000 `div` fps)

  -- Enter main loop
  mainGUI
  where
    fps = 30                                   --
    origin'@(ox:+oy)  = 20:+20                 --
    keysize'@(sx:+sy) = (4:+13) * 40           --
    winsize           = (sx*7:+sy) + 2*origin' --


-- | Initial application state
-- TODO: Piano range
initalstate :: Int -> Vector -> Vector -> AppState
initalstate nkeys origin' keysize' = AppState { _piano = PianoSettings { _origin  = origin',
                                                                         _keysize = keysize',
                                                                         _indent  = 0.26,
                                                                         _mid     = 0.62,
                                                                         _active  = Nothing,
                                                                         _keys    = replicate nkeys False },

                                                _animation = AnimationData { _frame = 0,
                                                                             _fps   = 30 },

                                                _inputstate = InputState { _mouse=0:+0, _keyboard=S.empty },
                                                _bindings = M.fromList [("Escape", Cairo.liftIO mainQuit)] }


-- | Just a little hello world snippet to make sure everything is set up properly.
goodbyeWorld :: IO ()
goodbyeWorld = do
  putStrLn "Hello world!"
  putStrLn "Counting down to launch..."
  forM_ [10,9..0] ((>> threadDelay (10^6)) . print)
  putStrLn "Launching. What have I done!?"
