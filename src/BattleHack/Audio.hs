
-- |
-- Module      : BattleHack.Audio
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



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module BattleHack.Audio where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe            --
import Foreign               -- Import the foreigners!
import Foreign.C.Types       --
import Control.Monad (liftM) --
import Control.Concurrent    --
import Control.Applicative

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM

import Sound.OpenAL.AL.BasicTypes ()
import Sound.OpenAL

import BattleHack.Types
import BattleHack.Utilities.Math



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
numSamples      = round . (fromIntegral sampleRate *)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
device :: IO (Maybe Device)
device = do
  mdevice <- openDevice Nothing -- Default audio device
  maybe (putStrLn "Failed to open audio device." >> return Nothing) (return . Just) mdevice


-- |
-- buffer :: Buffer
-- buffer = _


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

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
setup :: IO (Maybe (Context, Device))
setup = do
  mdevice  <- device
  mcontext <-  maybe (return Nothing) (flip createContext []) mdevice -- <$> mdevice

  currentContext $= mcontext
  return $ pure (,) <*> mcontext <*> mdevice


-- |
-- TODO: Simplify
makebuffer :: [Double] -> IO Buffer
makebuffer samples = do
  [buffer] <- genObjectNames 1
  mutvec <- V.thaw . V.fromList . map (pcm 16) $ samples
  imm <- V.freeze mutvec
  let (memory, size) = V.unsafeToForeignPtr0 imm in fillbuffer buffer memory (fromIntegral size)
  return buffer
  where
    format = Mono16
    fillbuffer buffer mem size = withForeignPtr mem $ \ptr -> bufferData buffer $= BufferData (MemoryRegion (ptr :: Ptr CInt) size) format sampleRate

-- |
stopall :: Source -> IO ()
stopall source = do
  count <- get $ buffersQueued source
  stop [source]
  unqueueBuffers source count
  return ()


-- |
note :: Source -> Double -> Double -> IO ()
note source frequency duration = do
  audiobuffers <- makebuffer (take (numSamples 2) $ sine frequency)

  queueBuffers source [audiobuffers]
  play [source]
  threadDelay (round duration * 10^6)
