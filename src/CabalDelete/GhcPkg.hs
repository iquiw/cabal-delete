module CabalDelete.GhcPkg
    ( getLibDir
    , ghcPkgList
    , unregisterPackage
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (get)
import System.Exit
import System.Process

import CabalDelete.Parse
import CabalDelete.Types
import CabalDelete.Utils

ghcPkgList :: (Functor m, MonadIO m) => CDM m PkgConfList
ghcPkgList = do
    s <- runGhcPkg [ "list" ]
    case parseGhcPkgList s of
        Right pc -> return pc
        Left err -> error $ show err

unregisterPackage :: (Functor m, MonadIO m) => PkgId -> CDM m ()
unregisterPackage pkgId = do
    result <- runGhcPkg [ "unregister", show pkgId ]
    msg result

runGhcPkg :: (Functor m, MonadIO m) => [String] -> CDM m String
runGhcPkg args = do
    f <- (scopeToFlag . cdScope) <$> get
    (code, out, err) <- liftIO $ readProcessWithExitCode "ghc-pkg" (f args) ""
    case code of
        ExitSuccess   -> return out
        ExitFailure n -> error $ "Exit with " ++ show n ++ ": " ++ err -- TODO

getLibDir :: IO FilePath
getLibDir = chomp <$> readProcess "ghc" ["--print-libdir"] ""
