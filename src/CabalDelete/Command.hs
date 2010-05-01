module CabalDelete.Command
    ( PackageEq
    , cmdInfo
    , cmdList
    , cmdListMinor
    , cmdNoDeps
    , cmdCheck
    , cmdDelete
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Ord
import Data.List
import Data.Version (showVersion)
import Distribution.InstalledPackageInfo
    ( description
    , depends
    , haddockHTMLs
    , importDirs
    , libraryDirs
    )
import Distribution.Package
    ( PackageId
    , PackageName(..)
    , packageName
    , packageVersion
    )
import GHC.Paths
import System.Directory
import System.FilePath

import CabalDelete.GhcPkg
import CabalDelete.ReverseDepends
import CabalDelete.Types
import CabalDelete.Utils

data PathResult =
      PathOK
    | PathNotFound
    | PathCommon
    | PathGhc
    deriving (Eq)

instance Show PathResult where
    showsPrec _ PathOK       = ("[D] " ++)
    showsPrec _ PathNotFound = ("[N] " ++)
    showsPrec _ PathCommon   = ("[I] " ++)
    showsPrec _ PathGhc      = ("[A] " ++)

cmdInfo :: [String] -> IO ()
cmdInfo names = withRevDepends $ forM_ names $ \n -> do
    rds <- resolveName n
    case rds of
        [] -> msg "No package found."
        _  -> mapM_ printInfo rds
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
        msg  $ alignDList len "ReverseDepends: " (map show $ rdRDepends rd)

cmdList :: IO ()
cmdList = do
    gs <- getPkgGroups (==.)
    case gs of
        [] -> putStrLn "There is no package with multiple versions."
        _  -> do
            putStrLn "The following packages have multiple versions."
            putStrLn ""
            printPkgVers gs

cmdListMinor :: IO ()
cmdListMinor = do
    gs <- getPkgGroups (.==)
    case gs of
        [] -> putStrLn "There is no package with multiple minor versions."
        _  -> do
            putStrLn "The following packages have multiple minor versions."
            putStrLn ""
            printPkgVers gs

getPkgGroups :: PackageEq -> IO [[PackageId]]
getPkgGroups eq = do
    ps <- getPackages `fmap` ghcPkgList
    return [ g | g <- groupBy eq $ sort ps, length g >= 2 ]

printPkgVers :: [[PackageId]] -> IO ()
printPkgVers [] = return ()
printPkgVers gs = do
    let ns = align $ map (name . packageName . head) gs
    mapM_ putStrLn $ zipWith (\n vs -> n ++ ": " ++ vers vs) ns gs
  where
    name (PackageName n) = n

    vers = unwords . map (showVersion . packageVersion)

    align xs = let m = maximum $ map length xs
               in map (take (m + 1) . (++repeat ' ')) xs

cmdNoDeps :: IO ()
cmdNoDeps = withRevDepends $ do
    rds <- filterRevDepends (flip (const . null . rdRDepends))
    case rds of
        [] -> msg "All packages have reverse dependencies."
        _  -> do
            msg "The following packages have no reverse dependency."
            msg ""
            mapM_ (msg . show . toPkgId . rdPkgInfo) rds

cmdCheck :: [String] -> IO ()
cmdCheck names = do
    putStrLn "=== CHECK MODE. No package will be deleted actually. ==="
    withRevDepends $ forM_ names $ flip checkWith (deleteProc False)

cmdDelete :: [String] -> IO ()
cmdDelete names =
    withRevDepends $ forM_ names $ flip checkWith (deleteProc True)

deleteProc :: Bool -> RevDepends -> RevDependsM ()
deleteProc b rd =
    case rdRDepends rd of
        [] -> do
            paths <- liftIO $ getDeletePaths rd
            case paths of
                [] -> msg "No delete path found."
                _  -> do
                    msg "The following directories will be processed."
                    msg "    D: Delete, N: NotFound, I: Ignore, A: Abort"
            abort <- or `fmap` mapM msgPR paths
            when b $
                if abort
                then msg "Aborted."
                else askIf ("Do you want to delete "
                            ++ show (rdPkgId rd)
                            ++ " ? [Y/n] ")
                     (liftIO $ do
                           mapM_ deletePath paths
                           unregisterPackage $ rdPkgId rd
                           putStrLn $ show (rdPkgId rd) ++ " was deleted.")
                     (msg "Canceled.")
        ds -> do
            msg $ "The follwoing packages depend on " ++ show (rdPkgId rd)
            mapM_ (msg . show) ds
  where
    msgPR (r, p) = msg (show r ++ p)
                   >> if r == PathGhc then return True else return False

checkWith :: String -> (RevDepends -> RevDependsM a) -> RevDependsM [a]
checkWith name proc = do
    rds <- resolveName name
    case rds of
        []  -> msg "No package found." >> return []
        [_] -> mapM proc rds
        _   -> askIf ("Delete old versions of \"" ++ name ++ "\" ? [Y/n] ")
               (mapM proc (oldVers rds))
               (return [])
  where
    oldVers = tail . reverse . sortBy (comparing rdPkgId)

deletePath :: (PathResult, FilePath) -> IO ()
deletePath (PathOK, p) = do
    removeDirectoryRecursive p
    putStrLn $ "Directory " ++ p ++ " was deleted."
deletePath (PathNotFound, p)
    = putStrLn $ "Directory " ++ p ++ " was not found."
deletePath (PathGhc, p)
    = putStrLn $ "Directory " ++ p ++ " is a system directory."
deletePath (PathCommon, p) =
    putStrLn $ "Directory " ++ p ++ " does not contain package name."

getDeletePaths :: RevDepends -> IO [(PathResult, FilePath)]
getDeletePaths rd = 
    let funcs = [importDirs, libraryDirs, haddockHTMLs]
        paths = map norm $ nub $ concatMap ($ rdPkgInfo rd) funcs
    in mapM checkPath paths
  where
    norm p | "/." `isSuffixOf` p = normalise $ takeDirectory p
           | otherwise           = normalise p
    checkPath p = do
        b <- doesDirectoryExist p
        case b of
            False | "$topdir" `isPrefixOf` p ||
                    "$httptopdir" `isPrefixOf` p
                    -> return (PathGhc, p)
                  | otherwise
                    -> return (PathNotFound, p)
            True | norm libdir `isInfixOf` p ||
                   norm docdir `isInfixOf` p
                   -> return (PathGhc, p)
                 | show (rdPkgId rd) `isInfixOf` takeDirectory p
                   -> return (PathOK, takeDirectory p)
                 | otherwise
                   -> return (PathCommon, p)

