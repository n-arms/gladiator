{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_memory_management_strats (
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
bindir     = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/bin"
libdir     = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/lib/x86_64-osx-ghc-9.6.5/memory-management-strats-0.1.0.0-AhtVkTSF3jp74u9X8GxnQL"
dynlibdir  = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/lib/x86_64-osx-ghc-9.6.5"
datadir    = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/share/x86_64-osx-ghc-9.6.5/memory-management-strats-0.1.0.0"
libexecdir = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/libexec/x86_64-osx-ghc-9.6.5/memory-management-strats-0.1.0.0"
sysconfdir = "/Users/armstrong/Documents/code/haskell/gladiator/.stack-work/install/x86_64-osx/fcd199f6ca49569f08776b90cfd40b6a8b2b7fe965c18c5a062a60bf5b382b7f/9.6.5/etc"

getBinDir     = catchIO (getEnv "memory_management_strats_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "memory_management_strats_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "memory_management_strats_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "memory_management_strats_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memory_management_strats_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memory_management_strats_sysconfdir") (\_ -> return sysconfdir)



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
