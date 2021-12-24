{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_wai_extra (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,1,7] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/bbw/.cabal/store/ghc-8.10.7/w-xtr-3.1.7-df6a160c/bin"
libdir     = "/Users/bbw/.cabal/store/ghc-8.10.7/w-xtr-3.1.7-df6a160c/lib"
dynlibdir  = "/Users/bbw/.cabal/store/ghc-8.10.7/lib"
datadir    = "/Users/bbw/.cabal/store/ghc-8.10.7/w-xtr-3.1.7-df6a160c/share"
libexecdir = "/Users/bbw/.cabal/store/ghc-8.10.7/w-xtr-3.1.7-df6a160c/libexec"
sysconfdir = "/Users/bbw/.cabal/store/ghc-8.10.7/w-xtr-3.1.7-df6a160c/etc"

getBinDir     = catchIO (getEnv "wai_extra_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "wai_extra_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "wai_extra_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "wai_extra_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wai_extra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wai_extra_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
