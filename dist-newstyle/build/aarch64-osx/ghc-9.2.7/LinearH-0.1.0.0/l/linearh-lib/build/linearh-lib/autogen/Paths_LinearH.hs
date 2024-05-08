{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_LinearH (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/parthdhanotra/.cabal/bin"
libdir     = "/Users/parthdhanotra/.cabal/lib/aarch64-osx-ghc-9.2.7/LinearH-0.1.0.0-inplace-linearh-lib"
dynlibdir  = "/Users/parthdhanotra/.cabal/lib/aarch64-osx-ghc-9.2.7"
datadir    = "/Users/parthdhanotra/.cabal/share/aarch64-osx-ghc-9.2.7/LinearH-0.1.0.0"
libexecdir = "/Users/parthdhanotra/.cabal/libexec/aarch64-osx-ghc-9.2.7/LinearH-0.1.0.0"
sysconfdir = "/Users/parthdhanotra/.cabal/etc"

getBinDir     = catchIO (getEnv "LinearH_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "LinearH_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "LinearH_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "LinearH_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LinearH_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LinearH_sysconfdir") (\_ -> return sysconfdir)




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
