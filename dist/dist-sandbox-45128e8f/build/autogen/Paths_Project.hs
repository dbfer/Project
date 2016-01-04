module Paths_Project (
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

bindir     = "/Users/Fred/Documents/Study/haskell/Project/Project/Project/Project/.cabal-sandbox/bin"
libdir     = "/Users/Fred/Documents/Study/haskell/Project/Project/Project/Project/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/Project-0.1.0.0-8N2b4FQtDfeHHu3js5RQwV"
datadir    = "/Users/Fred/Documents/Study/haskell/Project/Project/Project/Project/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/Project-0.1.0.0"
libexecdir = "/Users/Fred/Documents/Study/haskell/Project/Project/Project/Project/.cabal-sandbox/libexec"
sysconfdir = "/Users/Fred/Documents/Study/haskell/Project/Project/Project/Project/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
