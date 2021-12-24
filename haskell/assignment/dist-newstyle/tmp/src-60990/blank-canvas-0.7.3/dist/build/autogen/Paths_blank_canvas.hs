{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_blank_canvas (
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
version = Version [0,7,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/bbw/.cabal/store/ghc-8.10.7/blnk-cnvs-0.7.3-1363b36c/bin"
libdir     = "/Users/bbw/.cabal/store/ghc-8.10.7/blnk-cnvs-0.7.3-1363b36c/lib"
dynlibdir  = "/Users/bbw/.cabal/store/ghc-8.10.7/lib"
datadir    = "/Users/bbw/.cabal/store/ghc-8.10.7/blnk-cnvs-0.7.3-1363b36c/share"
libexecdir = "/Users/bbw/.cabal/store/ghc-8.10.7/blnk-cnvs-0.7.3-1363b36c/libexec"
sysconfdir = "/Users/bbw/.cabal/store/ghc-8.10.7/blnk-cnvs-0.7.3-1363b36c/etc"

getBinDir     = catchIO (getEnv "blank_canvas_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "blank_canvas_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "blank_canvas_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "blank_canvas_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blank_canvas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blank_canvas_sysconfdir") (\_ -> return sysconfdir)




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
