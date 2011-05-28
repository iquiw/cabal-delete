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
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Package hiding (depends)
import Distribution.Verbosity

import CabalDelete.Types
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
    rd <- RDM `fmap` load
    rd `deepseq` runReaderT proc rd

load :: IO (Map PkgId RevDepends)
load = do
    (compiler, pcfg) <- configCompiler (Just GHC) Nothing Nothing
                                       defaultProgramConfiguration normal
    pidx <- getInstalledPackages normal compiler
                                  [GlobalPackageDB, UserPackageDB] pcfg
#if MIN_VERSION_Cabal(1,10,0)
    return $ M.fromList $ r $ allPackages $ pidx
#else
    return $ maybe M.empty (M.fromList . r . allPackages) pidx
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
    f k _ = n =-= packageName (packageId k)

revDependsById :: PackageId -> RevDependsM [RevDepends]
revDependsById i = filterRevDepends f
  where
    f k _ = packageId k =-= i

revDependsByKey :: PkgKey -> RevDependsM [RevDepends]
revDependsByKey pk = filterRevDepends f
  where
    f k _ = pkgKey k =-= pk
