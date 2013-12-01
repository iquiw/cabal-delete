{-# LANGUAGE CPP #-}
module CabalDelete.ReverseDepends
    ( RevDepends(rdPkgInfo, rdRDepends)
    , RevDependsM
    , rdPkgId
    , reload
    , withRevDepends
    , resolveName
    , filterRevDepends
    , revDependsById
    , revDependsByKey
    , revDependsList
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT, get, put)
import Data.Map (Map)
import qualified Data.Map as M
import Distribution.InstalledPackageInfo
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Package hiding (depends)
import Distribution.Verbosity

import CabalDelete.Types
import CabalDelete.Parse

data RevDepends = RD
    { rdPkgInfo  :: InstalledPackageInfo
    , rdRDepends :: [PkgId]
    }

rdPkgId :: RevDepends -> PkgId
rdPkgId = toPkgId . rdPkgInfo

data RevDependsMap = RDM
    { rdmCache :: Map PkgId RevDepends
    , rdmIndex :: PackageIndex
    }

type RevDependsM = StateT RevDependsMap IO

withRevDepends :: RevDependsM a -> IO a
withRevDepends proc = do
    rdm <- uncurry RDM <$> liftIO load
    evalStateT proc rdm

load :: IO (Map PkgId RevDepends, PackageIndex)
load = do
#if MIN_VERSION_Cabal(1,18,0)
    (compiler, _, pcfg) <- configCompilerEx (Just GHC) Nothing Nothing
                           defaultProgramConfiguration normal
#else
    (compiler, pcfg) <- configCompiler (Just GHC) Nothing Nothing
                        defaultProgramConfiguration normal
#endif
    pidx <- getInstalledPackages normal compiler
            [GlobalPackageDB, UserPackageDB] pcfg
#if MIN_VERSION_Cabal(1,10,0)
    return (M.fromList $ r $ allPackages pidx, pidx)
#else
    case pidx of
        Just pidx' -> return (M.fromList $ r $ allPackages pidx', pidx')
        Nothing    -> return (M.empty, fromList [])
#endif
  where
    r ps = [ (toPkgId p, RD p (d (installedPackageId p) ps)) | p <- ps ]
    d i ps = [ toPkgId p | p <- ps, i `elem` depends p ]

reload :: RevDependsM ()
reload = do
    rdm <- liftIO $ uncurry RDM <$> load
    put rdm

resolveName :: String -> RevDependsM [RevDepends]
resolveName name =
    case parsePkgId name of
        Right pkgId -> revDependsById pkgId
        Left _      -> revDependsByName (PackageName name)

filterRevDepends :: (PkgId -> RevDepends -> Bool) -> RevDependsM [RevDepends]
filterRevDepends f =
    (map snd . M.toList . M.filterWithKey f . rdmCache) <$> get

revDependsByName :: PackageName -> RevDependsM [RevDepends]
revDependsByName n = filterRevDepends f
  where
    f k _ = n =-= packageName (packageId k)

revDependsById :: PackageId -> RevDependsM [RevDepends]
revDependsById i = filterRevDepends f
  where
    f k _ = packageId k =-= i

revDependsByKey :: InstalledPackageId -> RevDependsM [RevDepends]
revDependsByKey pk = filterRevDepends f
  where
    f k _ = piInstalledId k == pk

revDependsList :: [PkgId] -> RevDependsM [PkgId]
revDependsList pis = do
    pidx <- rdmIndex <$> get
    let m = reverseDependencyClosure pidx (map piInstalledId pis)
    return $ map toPkgId $ topologicalOrder $ fromList m
