{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_http2 (
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
version = Version [3,0,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/bbw/.cabal/store/ghc-8.10.7/http2-3.0.2-91128410/bin"
libdir     = "/Users/bbw/.cabal/store/ghc-8.10.7/http2-3.0.2-91128410/lib"
dynlibdir  = "/Users/bbw/.cabal/store/ghc-8.10.7/lib"
datadir    = "/Users/bbw/.cabal/store/ghc-8.10.7/http2-3.0.2-91128410/share"
libexecdir = "/Users/bbw/.cabal/store/ghc-8.10.7/http2-3.0.2-91128410/libexec"
sysconfdir = "/Users/bbw/.cabal/store/ghc-8.10.7/http2-3.0.2-91128410/etc"

getBinDir     = catchIO (getEnv "http2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "http2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "http2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "http2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "http2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "http2_sysconfdir") (\_ -> return sysconfdir)




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
