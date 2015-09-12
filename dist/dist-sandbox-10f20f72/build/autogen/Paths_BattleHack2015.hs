module Paths_BattleHack2015 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\jonat\\Desktop\\BattleHack-2015\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\jonat\\Desktop\\BattleHack-2015\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\BattleHack2015-0.1.0.0-Byy5c7CtsZM9KY1Y6F9fAf"
datadir    = "C:\\Users\\jonat\\Desktop\\BattleHack-2015\\.cabal-sandbox\\x86_64-windows-ghc-7.10.2\\BattleHack2015-0.1.0.0"
libexecdir = "C:\\Users\\jonat\\Desktop\\BattleHack-2015\\.cabal-sandbox\\BattleHack2015-0.1.0.0-Byy5c7CtsZM9KY1Y6F9fAf"
sysconfdir = "C:\\Users\\jonat\\Desktop\\BattleHack-2015\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BattleHack2015_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BattleHack2015_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "BattleHack2015_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BattleHack2015_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BattleHack2015_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
