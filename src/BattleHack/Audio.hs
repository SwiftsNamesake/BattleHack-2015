
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
import Data.Maybe
import Foreign                   -- Import the foreigners!
import Foreign.C.Types           --
import Control.Monad (liftM)
import Control.Concurrent

import qualified Data.Vector.Storable as V          --
import qualified Data.Vector.Storable.Mutable as VM --

import Sound.OpenAL.AL.BasicTypes ()        --
import Sound.OpenAL



sampleRate :: Num n => n
sampleRate = 44100

type Sample = Double

-- bufferData audiobuffer $= (BufferData _ Mono16 44100)


--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
bufferSize :: Storable a => Int -> a -> Double -> Int
bufferSize nchannels sampleType secs = fromIntegral (numSamples secs) * sizeOf sampleType * nchannels


-- |
numSamples :: Integral n => Double -> n --NumSamples
numSamples secs = round (fromIntegral sampleRate * secs)

---------------------------------------------------------------------------------------------------
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
-- # From http://dev.stephendiehl.com/hask/#ffi
vecPtr :: VM.MVector s CInt -> ForeignPtr CInt
vecPtr = fst . VM.unsafeToForeignPtr0

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
  where
    d  = 2 * pi * freq / sr
    n  = truncate (sr /freq)
    sr = fromIntegral sampleRate

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
setup :: IO (Maybe Device)
setup = do
  mdevice  <- device
  mcontext <- maybe (return Nothing) (flip createContext []) mdevice

  currentContext $= mcontext
  return mdevice


-- |
makebuffer :: [Double] -> IO Buffer
makebuffer samples = do
  [buffer] <- genObjectNames 1
  mutableV <- V.thaw . V.fromList . map (pcm 16) $ samples -- . take (length samples) $ --sine 220  --
  ptr      <- V.freeze mutableV
  let (mem, size) = V.unsafeToForeignPtr0 ptr --
  withForeignPtr mem $ \ptr -> bufferData buffer $= BufferData (MemoryRegion (ptr :: Ptr CInt) (fromIntegral size)) Mono16 sampleRate           --
  return buffer

-- |
note :: Device -> Double -> Double -> IO ()
note device frequency duration = do
  audiobuffers <- makebuffer (take (numSamples 2) $ sine frequency)
  [source]     <- genObjectNames 1

  loopingMode source $= Looping

  queueBuffers source [audiobuffers]
  play [source]
  threadDelay (round duration * 10^6)
