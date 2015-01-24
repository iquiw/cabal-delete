{-# LANGUAGE CPP #-}
module CabalDelete.Types
    ( CDCmd(..)
    , CDConfig(..)
    , CDM
    , runCDM
    , PkgConfList
    , PkgId(..)
    , PackageScope(..)
    , (=-=)
    , (.==)
    , getPackageIndex
    , toPkgId
    , scopeToFlag
    , showsPackageId
#if !MIN_VERSION_Cabal(1, 22, 0)
    , InstalledPackageIndex
#endif
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Char (toLower)
import Data.Function (on)
import Data.Version (Version(..), showVersion)
import Distribution.InstalledPackageInfo
    (InstalledPackageInfo, installedPackageId, sourcePackageId)
import Distribution.Package
    ( InstalledPackageId(..)
    , Package(..)
    , PackageId
    , PackageIdentifier(..)
    , PackageName(..)
    )
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.GHC (getInstalledPackages, getPackageDBContents)
#if MIN_VERSION_Cabal(1, 22, 0)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
#else
import Distribution.Simple.PackageIndex (PackageIndex)
#endif
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Verbosity (Verbosity)

#if !MIN_VERSION_Cabal(1, 22, 0)
type InstalledPackageIndex = PackageIndex
#endif

data CDCmd
    = CmdHelp
    | CmdInfo
    | CmdList
    | CmdDelete
    | CmdNoDeps
    | CmdVersion
    deriving (Eq, Show, Ord)

data PackageScope = ScopeUser | ScopeGlobal | ScopeAll deriving (Eq, Show)

data CDConfig = CDConfig
    { cdCmd       :: CDCmd
    , cdDryRun    :: Bool
    , cdGhcLibdir :: FilePath
    , cdMinorOnly :: Bool
    , cdRecursive :: Bool
    , cdScope     :: PackageScope
    , cdYesToAll  :: Bool
    }

type CDM m = StateT CDConfig m

runCDM :: (Monad m) => CDM m a -> CDConfig -> m a
runCDM = evalStateT

type PkgConfList = [(FilePath, [PackageId])]

class EqIC a where
    (=-=) :: a -> a -> Bool

instance EqIC Char where
    (=-=) = (==) `on` toLower

instance EqIC a => EqIC [a] where
    (=-=) [] [] = True
    (=-=) _  [] = False
    (=-=) [] _  = False
    (=-=) (x:xs) (y:ys) = (x =-= y) && xs =-= ys

instance EqIC PackageName where
    (=-=) (PackageName n1) (PackageName n2) = n1 =-= n2

instance EqIC PackageIdentifier where
    (=-=) (PackageIdentifier n1 v1) (PackageIdentifier n2 v2) =
        n1 =-= n2 && v1 == v2

data PkgId = PkgId
    { piInstalledId :: InstalledPackageId
    , piSourceId :: PackageIdentifier
    } deriving Eq

instance Package PkgId where
    packageId (PkgId _ si) = si

instance Show PkgId where
    showsPrec _ (PkgId _ si) = showsPackageId si

instance Ord PkgId where
    compare (PkgId _ si1) (PkgId _ si2) = compare si1 si2

-- | returns True if packages' major versions are same.
(.==) :: PackageId -> PackageId -> Bool
(.==) (PackageIdentifier (PackageName n1) (Version (v1:v1':_) _))
      (PackageIdentifier (PackageName n2) (Version (v2:v2':_) _))
    = n1 =-= n2 && v1 == v2 && v1' == v2'
(.==) _ _ = False

showsPackageId :: PackageId -> ShowS
showsPackageId (PackageIdentifier (PackageName n) v) =
    (n ++) . ("-" ++) . (showVersion v ++)

toPkgId :: InstalledPackageInfo -> PkgId
toPkgId = PkgId <$> installedPackageId <*> sourcePackageId

scopeToFlag :: PackageScope -> [String] -> [String]
scopeToFlag ScopeUser   = ("--user":)
scopeToFlag ScopeGlobal = ("--global":)
scopeToFlag ScopeAll    = id

getPackageIndex :: PackageScope -> Verbosity -> ProgramConfiguration
                -> IO InstalledPackageIndex
getPackageIndex ScopeAll v pcfg =
    getInstalledPackages v [GlobalPackageDB, UserPackageDB] pcfg
getPackageIndex scope v pcfg =
    getPackageDBContents v
    (if scope == ScopeUser then UserPackageDB else GlobalPackageDB) pcfg
