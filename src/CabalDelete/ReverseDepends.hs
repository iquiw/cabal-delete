{-# LANGUAGE CPP #-}
module CabalDelete.ReverseDepends
    ( PackageInfo
    , RevDepends(rdPkgInfo, rdRDepends)
    , RevDependsM
    , rdPkgId
    , withRevDepends
    , resolveName
    , filterRevDepends
    , revDependsByKey
    ) where

import Control.DeepSeq
import Control.Monad.Trans.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Distribution.InstalledPackageInfo
#if __GLASGOW_HASKELL__ >= 612
import Distribution.InstalledPackageInfo.Binary
#endif
import Distribution.Package hiding (depends)

import CabalDelete.Types
import CabalDelete.GhcPkg
import CabalDelete.Parse

data RevDepends = RD
    { rdPkgInfo  :: PackageInfo
    , rdRDepends :: [PkgId]
    }

rdPkgId :: RevDepends -> PkgId
rdPkgId = toPkgId . rdPkgInfo

data RevDependsMap = RDM
    { cache :: Map PkgId RevDepends
    }

instance NFData RevDependsMap

type RevDependsM = ReaderT RevDependsMap IO

withRevDepends :: RevDependsM a -> IO a
withRevDepends proc = do
    pl <- ghcPkgList
    rd <- (RDM . M.unions) `fmap` mapM load (getPkgConfs pl)
    rd `deepseq` runReaderT proc rd

load :: FilePath -> IO (Map PkgId RevDepends)
load path = do
#if __GLASGOW_HASKELL__ >= 612
    ps <- readBinPackageDB path :: IO [PackageInfo]
    return $ M.fromList $ r ps
#else
    x <- readFile path
    case x `deepseq` reads x :: [([PackageInfo], String)] of
        [(ps, _)] -> return $ M.fromList $ r ps
        _         -> error "Unable to load package.conf"
#endif
  where
    r ps = [ (toPkgId p, RD p (d (pkgKey $ toPkgId p) ps)) | p <- ps ]
    d i ps = [ toPkgId p | p <- ps, i `elem` depends p ]

resolveName :: String -> RevDependsM [RevDepends]
resolveName name =
    case parsePkgId name of
        Right pkgId -> revDependsById pkgId
        Left _      -> revDependsByName (PackageName name)

filterRevDepends :: (PkgId -> RevDepends -> Bool) -> RevDependsM [RevDepends]
filterRevDepends f =
    (map snd . M.toList . M.filterWithKey f . cache) `fmap` ask

revDependsByName :: PackageName -> RevDependsM [RevDepends]
revDependsByName n = filterRevDepends f
  where
    f k _ = n == packageName (packageId k)

revDependsById :: PackageId -> RevDependsM [RevDepends]
revDependsById i = filterRevDepends f
  where
    f k _ = packageId k == i

revDependsByKey :: PkgKey -> RevDependsM [RevDepends]
revDependsByKey pk = filterRevDepends f
  where
    f k _ = pkgKey k == pk
