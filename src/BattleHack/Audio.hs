
-- |
-- Module      : BattleHack.Audio
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX

-- Created September 12 2015

-- TODO | - Streaming service in separate thread (use conduit or mvars)
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Audio where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe                        --
import Data.List  (findIndices, transpose)          --
import Foreign hiding (void)             -- Import the foreigners!
import Foreign.C.Types                   --
import Control.Concurrent                --
import Control.Applicative               --
import Control.Monad (liftM, void, forM, when) --
import Control.Monad.Loops

import Text.Printf

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM

import Sound.OpenAL.AL.BasicTypes ()
import Sound.OpenAL
-- import Sound.ALUT   as Alut

import BattleHack.Types
import BattleHack.Utilities.Math
import qualified BattleHack.Piano as Piano



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
sampleRate :: Num n => n
sampleRate = 44100



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- bufferSize :: Storable a => Int -> a -> Double -> Int
-- bufferSize nchannels sampleType secs = fromIntegral (numSamples secs) * sizeOf sampleType * nchannels


-- |
numSamples :: Integral n => Double -> n --NumSamples
numSamples = round . (fromIntegral sampleRate *)
-- numSamples secs = round (fromIntegral sampleRate * secs)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
device :: IO (Maybe Device)
device = do
  mdevice <- openDevice Nothing -- Default audio device
  maybe (putStrLn "Failed to open audio device." >> return Nothing) (return . Just) mdevice


-- |
setup :: IO (Maybe (Context, Device))
setup = do
  mdevice  <- device
  mcontext <-  maybe (return Nothing) (flip createContext []) mdevice -- <$> mdevice

  currentContext $= mcontext
  return $ pure (,) <*> mcontext <*> mdevice


-- |
pcm :: (Integral a) => Int -> Sample -> a
pcm bits sample = truncate $ sample * (fromIntegral $ ((2 :: Int) ^ (bits - 1)) - 1)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle . take n $ map sin [0, d..]
  where
    d  = 2 * pi * freq / sr      --
    n  = truncate (sr /freq)     --
    sr = fromIntegral sampleRate --


-- |
mix :: RealFloat r => [[r]] -> [r]
-- mix = map sum
mix = map (min 1.0 . sum)

-- Streaming -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Streamer type (?)
-- TODO: Pause, resume, stop
-- TODO: Set looping mode
-- TODO: Who has the control (use timer, or block via mvar?)
-- TODO: 'Double-buffering'
-- TODO: In what order are buffers removed and queued (?)
-- TODO: What happens when the same buffer is queued multiple times
-- TODO: What are OpenAL buffers specifically, what happens when you write to a queued buffer (?)
-- TODO: Do you have to align sine waves properly (ie. line up the periods) (?)
-- TODO: Do I have to free memory manually (?)
-- TODO: Generalise (eg. any frame type, any container besides lists, any number of buffers, etc.)
-- TODO: Use frame count instead of dt (more precise) (?)
-- https://hackage.haskell.org/package/OpenAL-1.7.0.1/docs/Sound-OpenAL-AL-Source.html
-- stream :: Double -> Source -> (a -> [[CInt]]) -> MVar a -> IO ()
-- stream :: Double -> Source -> MVar [Bool] -> IO ()
stream :: Double -> MVar [Bool] -> IO ()
stream dt mnotes = do

  -- Maybe we have to init OpenAL in this thread?
  printf "Entering streaming function.\n"
  Just (context, device) <- setup -- TODO: Return context as well (probably a good idea) (✓)
  [source] <- genObjectNames 1

  --
  [primero, segundo] <- genObjectNames 2 --
  print primero
  print segundo

  --
  nextbatch source primero
  -- play [source]

  -- loopingMode source $= Looping

  --
  when True $ do
    forM (cycle [segundo, primero]) $ \buffer -> do
      putStrLn "New streaming iteration"
      nextbatch source buffer
      -- stop [source]
      -- queueBuffers source [buffer]
      -- play [source]
      putStrLn "Status:"
      nprocessed <- get (buffersProcessed source)
      print nprocessed
      nqueued <- get (buffersQueued source)
      print nqueued

      old <- unqueueBuffers source nprocessed
      print (primero, segundo, old, buffer)
      -- print . fromIntegral <$> get (buffersProcessed source)
      -- print . fromIntegral <$> get (buffersQueued source)
      -- old <- unqueueBuffers source 1
      -- print $ old == [buffer]
      -- print $ old
      -- play [source]
    -- threadDelay . floor $ dt * 10^6
    return ()
  return ()
  where
    takeplaying = findIndices id
    format      = Mono16
    mixnotes [] = replicate (numSamples dt) 0
    mixnotes n  = mix . take (numSamples dt) . transpose . map (sine . Piano.pitchFromKeyIndex) $ n
    -- mixnotes    = mix . take (numSamples dt) . transpose . map (sine . Piano.pitchFromKeyIndex) . take 1
    -- mixnotes = map sin . map Piano.pitchFromKeyIndex
    -- mixnotes    = (take (numSamples dt) . sine . Piano.pitchFromKeyIndex) . head
    nextbatch source buffer = do
      putStrLn "Processing batch"
      presses <- take 1 . takeplaying <$> takeMVar mnotes
      -- presses <- takeplaying <$> return [False, False, False, False, True, False, False, False, True]
      printf "Mixing %d notes.\n" (length presses)
      -- print $ mixnotes $ presses
      fillbufferWithSamples buffer format $ mixnotes presses
      queueBuffers source [buffer]
      play [source]


-- Control ---------------------------------------------------------------------------------------------------------------------------------

-- |
playnote :: [(Source, Buffer)] -> Int -> IO ()
playnote keyboard i = play [fst $ keyboard !! i]


-- |
stopnote :: [(Source, Buffer)] -> Int -> IO ()
stopnote keyboard i = stop [fst $ keyboard !! i]


-- Buffers ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Simplify
-- TODO: Don't hard-code format
-- TODO: Let fillbuffer take care of the pointer stuff
makebuffer :: [Double] -> IO Buffer
makebuffer samples = do
  [buffer] <- genObjectNames 1
  fillbufferWithSamples buffer format samples
  where
    format = Mono16


-- |
-- TODO: Take a closer look at pcm (eg. why 16?)
fillbufferWithSamples :: Buffer -> Format -> [Double] -> IO Buffer
fillbufferWithSamples buffer format samples = do
  mutvec <- V.thaw . V.fromList . map (pcm 16) $ samples
  imm <- V.freeze mutvec
  let (memory, size) = V.unsafeToForeignPtr0 imm in fillbuffer buffer format memory (fromIntegral size)
  return buffer


-- |
fillbuffer :: Buffer -> Format -> ForeignPtr CInt -> ALsizei -> IO ()
fillbuffer buffer format mem size = withForeignPtr mem $ \ptr -> bufferData buffer $= BufferData (MemoryRegion (ptr :: Ptr CInt) size) format sampleRate -- TODO: Factor out


-- |
-- TODO: Make 'keyboard' type (?)
-- TODO: Rename (eg. createKeyboard)
makebuffersFromIndeces :: [Int] -> IO [(Source, Buffer)]
makebuffersFromIndeces indeces = do
  pitches <- mapM (makebuffer . take (numSamples 5.0) . sine . Piano.pitchFromKeyIndex) indeces
  sources <- genObjectNames (length indeces)

  forM sources $ \source -> loopingMode source $= [OneShot, Looping] !! 1 -- Easy toggling

  mapM_ (\(source, pitch) -> queueBuffers source [pitch]) $ zip sources pitches
  return $ zip sources pitches


-- |
-- stopall :: Source -> IO ()
-- stopall source = do
--   count <- get $ buffersQueued source
--   stop [source]
--   void $ unqueueBuffers source count


-- |
-- note :: Source -> Double -> Double -> IO ()
-- note source frequency duration = do
--   audiobuffer <- makebuffer . take (numSamples duration) $ sine frequency
--
--   queueBuffers source [audiobuffer]
--   play [source]
  -- threadDelay (round duration * 10^6)
