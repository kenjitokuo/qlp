{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_qlp (
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
bindir     = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\bin"
libdir     = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\lib\\x86_64-windows-ghc-9.10.3-b42a\\qlp-0.1.0.0-EoacUnrnt7V8trGnX6j0RY-qlp"
dynlibdir  = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\lib\\x86_64-windows-ghc-9.10.3-b42a"
datadir    = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\share\\x86_64-windows-ghc-9.10.3-b42a\\qlp-0.1.0.0"
libexecdir = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\libexec\\x86_64-windows-ghc-9.10.3-b42a\\qlp-0.1.0.0"
sysconfdir = "C:\\Users\\yue\\qlp\\.stack-work\\install\\664dda1e\\etc"

getBinDir     = catchIO (getEnv "qlp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "qlp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "qlp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "qlp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "qlp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "qlp_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
