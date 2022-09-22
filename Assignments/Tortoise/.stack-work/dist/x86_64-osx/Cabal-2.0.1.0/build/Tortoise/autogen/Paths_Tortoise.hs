{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Tortoise (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/bin"
libdir     = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/lib/x86_64-osx-ghc-8.2.2/Tortoise-1.0-ITM6wFVzbnCFoH6Ahk9fip-Tortoise"
dynlibdir  = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/share/x86_64-osx-ghc-8.2.2/Tortoise-1.0"
libexecdir = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/libexec/x86_64-osx-ghc-8.2.2/Tortoise-1.0"
sysconfdir = "/Users/jmathias686/Documents/UNI STUFF/Year 6/T2/comp3141/Assignments/Tortoise/.stack-work/install/x86_64-osx/b549f3b7a80bb216bfbfee8ae9df2f34c304cca62c089f78785a8ad4fe32a285/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tortoise_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tortoise_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Tortoise_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Tortoise_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tortoise_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tortoise_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
