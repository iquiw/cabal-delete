module CabalDelete.Command
    ( cmdDelete
    , cmdInfo
    , cmdList
    , cmdNoDeps
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Monad (filterM, forM_, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get, modify)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (groupBy, sort, sortBy, nub, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Version (showVersion)
import Distribution.InstalledPackageInfo
    ( InstalledPackageInfo
    , description
    , depends
    , haddockHTMLs
    , importDirs
    , libraryDirs
    )
import Distribution.Package
    ( Package(..)
    , PackageName(..)
    , packageName
    , packageVersion
    )
import Distribution.System (OS(Windows), buildOS)
import GHC.Paths (docdir)
import System.Directory
    ( doesDirectoryExist
    , getDirectoryContents
    , removeDirectoryRecursive
    )
import System.FilePath ((</>), joinPath, normalise, splitPath, takeDirectory)

import CabalDelete.GhcPkg
import CabalDelete.Parse
import CabalDelete.ReverseDepends
import CabalDelete.Types
import CabalDelete.Utils

type Command m = [String] -> CDM m ()

data PathResult
    = PathOK
    | PathNotFound
    | PathCommon
    | PathIgnore
    | PathGhc
    deriving (Eq)


cmdInfo :: Command RevDependsM
cmdInfo names = lift $ forM_ names (\n -> do
    rds <- resolveName n
    case rds of
        [] -> msg "No package found."
        _  -> mapM_ printInfo rds)
  where
    printInfo rd = do
        let pinfo = rdPkgInfo rd
            len = 78
        dps <- mapM revDependsByKey $ depends pinfo

        msg' $ alignDList len "Name:           " [show $ toPkgId pinfo]
        msg' $ alignDList len "Description:    "
            (words $ unwords $ lines $ description pinfo)
        msg' $ alignDList len "Depends:        "
            (map (show . rdPkgId) $ concat dps)
        msg' $ alignDList len "ReverseDepends: " (map show $ rdRDepends rd)
        msg' $ alignDList len "LibraryDirs:    " (libraryDirs pinfo)
        msg' $ alignDList len "ImportDirs:     " (importDirs pinfo)
        msg  $ alignDList len "HaddockDirs:    " (haddockHTMLs pinfo)

cmdList :: Command IO
cmdList _ = do
    b <- minorOnly <$> get
    let m = if b then " minor " else " "
    gs <- getPkgGroups (if b then (.==) else (=-=) `on` packageName)
    case gs of
        [] -> msg $ "There is no package with multiple" ++ m ++ "versions."
        _  -> do
            msg $ "The following packages have multiple" ++ m ++ "versions."
            msg ""
            lift $ printPkgVers gs
  where
    getPkgGroups eq = do
        ps <- liftIO $ concatMap snd <$> ghcPkgList
        return [ g | g <- groupBy eq $ sort ps, length g >= 2 ]

    printPkgVers [] = return ()
    printPkgVers gs = do
        let ns = align $ map (name . packageName . head) gs
        mapM_ msg $ zipWith (\n vs -> n ++ ": " ++ vers vs) ns gs

    name (PackageName n) = n

    vers = unwords . map (showVersion . packageVersion)

    align xs = let m = maximum $ map length xs
               in map (take (m + 1) . (++repeat ' ')) xs

cmdNoDeps :: Command RevDependsM
cmdNoDeps _ = do
    rds <- lift $ filterRevDepends (flip (const . null . rdRDepends))
    case rds of
        [] -> msg "All packages have reverse dependencies."
        _  -> do
            msg "The following packages have no reverse dependency."
            msg ""
            mapM_ (msg . show . toPkgId . rdPkgInfo) rds

cmdDelete :: Command RevDependsM
cmdDelete names = do
    dir <- liftIO getLibDir
    modify (\x -> x { ghcLibdir = dir })
    n <- dryRun <$> get
    when n $ msg "=== CHECK MODE. No package will be deleted actually. ==="
    forM_ names $ flip checkWith deleteProc


deleteProc :: RevDepends -> CDM RevDependsM ()
deleteProc rd  = do
    r <- recursive <$> get
    if r then
        do
            pis <- lift $ revDependsList $ rdPkgId rd : rdRDepends rd
            msg "=== RECURSIVE MODE. The following packages will be deleted recursively. === "
            mapM_ (msg . show) pis
            askIf "Do you want to proceed?"
                (proceed $ map packageId pis)
                (return ())
        else void (deleteOne rd)
  where
    proceed []     = return ()
    proceed (i:is) = do
        rds <- lift $ revDependsById i
        case rds of
            [rd'] -> do
                b <- deleteOne rd'
                when b $ do
                    lift reload
                    proceed is
            _     ->
                error $ "Not Supported: mutilple packages with same version: "
                      ++ showsPackageId i []

deleteOne :: RevDepends -> CDM RevDependsM Bool
deleteOne rd = do
    del <- not . dryRun <$> get
    libdir <- ghcLibdir <$> get
    case rdRDepends rd of
        [] -> do
            paths <- liftIO $ getDeletePaths libdir $ rdPkgInfo rd
            case paths of
                [] -> msg "No delete path found."
                _  -> do
                    msg "The following directories will be processed."
                    msg "    D: Delete, N: NotFound, I: Ignore, A: Abort"
            abort <- or <$> mapM msgPR paths
            case () of
                _ | not del   -> return False
                  | abort     -> msg "Aborted." >> return False
                  | otherwise ->
                      askIf ("Do you want to delete "
                            ++ show (rdPkgId rd) ++ " ?")
                            (liftIO $ do
                                mapM_ deletePath paths
                                unregisterPackage $ rdPkgId rd
                                putStrLn $ show (rdPkgId rd) ++ " was deleted."
                                return True)
                            (msg "Canceled." >> return False)
        ds -> do
            msg $ "The follwoing packages depend on " ++ show (rdPkgId rd)
            mapM_ (msg . show) ds
            return False
  where
    msgPR (r, p) = msg (resStr r ++ p)
                   >> if r == PathGhc then return True else return False

    resStr PathOK       = "[D] "
    resStr PathNotFound = "[N] "
    resStr PathCommon   = "[I] "
    resStr PathIgnore   = "[I] "
    resStr PathGhc      = "[A] "


checkWith :: String
             -> (RevDepends -> CDM RevDependsM a)
             -> CDM RevDependsM [a]
checkWith name proc = do
    rds <- lift $ resolveName name
    case rds of
        []  -> msg "No package found." >> return []
        [_] -> mapM proc rds
        _   -> askIf ("Delete old versions of \"" ++ name ++ "\" ?")
               (mapM proc (oldVers rds))
               (return [])
  where
    oldVers = tail . reverse . sortBy (comparing rdPkgId)

deletePath :: (PathResult, FilePath) -> IO ()
deletePath (r, p) = do
    when (r == PathOK) $ removeDirectoryRecursive p
    putStrLn $ "Directory " ++ p ++ msgDone r
  where
    msgDone PathOK       = " was deleted."
    msgDone PathNotFound = " was not found."
    msgDone PathGhc      = " is a system directory."
    msgDone PathCommon   = " does not contain package name."
    msgDone PathIgnore   = " was not deleted since other version uses it."

getDeletePaths :: String -> InstalledPackageInfo -> IO [(PathResult, FilePath)]
getDeletePaths libdir pinfo = do
    let ldirs = (++) <$> libraryDirs <*> importDirs $ pinfo
    lpaths <- mapM (checkPath . norm) $ nub $ sort ldirs
    hpaths <- mapM (checkPath . norm) $ sharedDirs pinfo
    noother <- and <$> mapM noOtherVer lpaths
    if noother
        then return $ map (second takeDirectory) lpaths ++ hpaths
        else return $ lpaths ++ map ignore hpaths
  where
    norm p | "/." `isSuffixOf` p = normalise $ takeDirectory p
           | otherwise           = normalise p

    checkPath p = do
        b <- doesDirectoryExist p
        case b of
            False | "$topdir" `isPrefixOf` p || "$httptopdir" `isPrefixOf` p
                    -> return (PathGhc, p)
                  | otherwise
                    -> return (PathNotFound, p)
            True  | norm libdir `isInfixOf` p || norm docdir `isInfixOf` p
                    -> return (PathGhc, p)
                  | show (toPkgId pinfo) `isInfixOf` p
                    -> return (PathOK, p)
                  | otherwise
                    -> return (PathCommon, p)

    noOtherVer (PathOK, p) = do
        fs <- filter isGHC <$> getDirectoryContents (takeDirectory p)
        l <- length <$> filterM (doesDirectoryExist . (takeDirectory p </>)) fs
        return $ l == 1
    noOtherVer _ = return True

    isGHC f = case parsePkgId f of
        Right i -> packageName i == PackageName "ghc"
        Left _  -> False

    ignore = first (const PathIgnore)

-- | Returns shared directories in which Haddock and other documents, such as
-- LICENSE, README, etc., are installed.
-- On Windows, other documents are installed in the same directory as
-- "html" directory exists.
-- On Unix, they are installed in "share" directory above the "doc" directory.
sharedDirs :: InstalledPackageInfo -> [FilePath]
sharedDirs pinfo = concatMap go $ haddockHTMLs pinfo
  where
    go p | "html" `isSuffixOf` p = let p' = takeDirectory p
                                   in if buildOS == Windows
                                      then [p']
                                      else [p', removeDoc p']
         | otherwise             = [p]

    removeDoc = joinPath . filter (/= "doc/") . splitPath
