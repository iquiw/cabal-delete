module CabalDelete.GhcPkg
    ( ghcPkgList
    , unregisterPackage
    ) where

import GHC.Paths
import System.Exit
import System.Process

import CabalDelete.Parse
import CabalDelete.Types

ghcPkgList :: IO PkgConfList
ghcPkgList = do
    s <- runGhcPkg [ "list" ]
    case parseGhcPkgList s of
        Right pc -> return pc
        Left err -> error $ show err

unregisterPackage :: PkgId -> IO ()
unregisterPackage pkgId =
    putStr =<< runGhcPkg [ "unregister", show pkgId ]

runGhcPkg :: [String] -> IO String
runGhcPkg args = do
    (code, out, err) <- readProcessWithExitCode ghc_pkg args ""
    case code of
        ExitSuccess   -> return out
        ExitFailure n -> error $ "Exit with " ++ show n ++ ": " ++ err -- TODO
