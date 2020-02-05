{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_project (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\bin"
libdir     = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\lib\\x86_64-windows-ghc-8.6.5\\project-0.0.0-4aYgMKf9k9iKwIJafvWxmN"
dynlibdir  = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\share\\x86_64-windows-ghc-8.6.5\\project-0.0.0"
libexecdir = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\libexec\\x86_64-windows-ghc-8.6.5\\project-0.0.0"
sysconfdir = "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\RT\\project\\.stack-work\\install\\654eef7a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
