{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_assignment (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bbw/.cabal/bin"
libdir     = "/home/bbw/.cabal/lib/x86_64-linux-ghc-8.6.5/assignment-0.1.0.0-KemWkyVw5xRD51kviw4wDv-assignment"
dynlibdir  = "/home/bbw/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bbw/.cabal/share/x86_64-linux-ghc-8.6.5/assignment-0.1.0.0"
libexecdir = "/home/bbw/.cabal/libexec/x86_64-linux-ghc-8.6.5/assignment-0.1.0.0"
sysconfdir = "/home/bbw/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assignment_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assignment_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "assignment_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "assignment_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assignment_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assignment_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
