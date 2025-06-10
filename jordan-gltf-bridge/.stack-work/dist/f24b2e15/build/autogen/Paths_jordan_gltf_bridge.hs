{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_jordan_gltf_bridge (
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
bindir     = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\bin"
libdir     = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\lib\\x86_64-windows-ghc-9.8.4\\jordan-gltf-bridge-0.1.0.0-EB5ZUJT6UgC2ACqxIE3AIO"
dynlibdir  = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\share\\x86_64-windows-ghc-9.8.4\\jordan-gltf-bridge-0.1.0.0"
libexecdir = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\libexec\\x86_64-windows-ghc-9.8.4\\jordan-gltf-bridge-0.1.0.0"
sysconfdir = "C:\\Users\\Public\\document\\tsushin\\how_to_create_-\\jordan-gltf-bridge\\.stack-work\\install\\e3016c07\\etc"

getBinDir     = catchIO (getEnv "jordan_gltf_bridge_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "jordan_gltf_bridge_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "jordan_gltf_bridge_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "jordan_gltf_bridge_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jordan_gltf_bridge_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jordan_gltf_bridge_sysconfdir") (\_ -> return sysconfdir)



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
