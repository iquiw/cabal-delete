{-# LANGUAGE CPP #-}
module CabalDelete.Types
    (
      CDCmd(..)
    , CDConfig(..)
    , CDM
    , runCDM
    , PkgConfList
    , PkgId(..)
    , PackageScope(..)
    , (=-=)
    , (.==)
    , getPackageIndex
    , pkgPath
    , toPkgId
    , scopeToFlag
    , showsPackageId
#if !MIN_VERSION_Cabal(1, 22, 0)
    , InstalledPackageIndex
#endif
    ) where

import Prelude hiding ((<*>))
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Char (toLower)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Version (Version(..), showVersion)
import Distribution.InstalledPackageInfo
    (
      InstalledPackageInfo
    , installedPackageId
    , sourcePackageId
    )
import Distribution.Package
    (
      InstalledPackageId(..)
    , Package(..)
    , PackageId
    , PackageIdentifier(..)
    , PackageName(..)
    )
import Distribution.Simple.Compiler (PackageDB(..) , CompilerFlavor(..))
import Distribution.Simple.Configure (configCompilerEx)
import Distribution.Simple.GHC (getInstalledPackages, getPackageDBContents)
#if MIN_VERSION_Cabal(1, 22, 0)
import Distribution.InstalledPackageInfo (packageKey)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Compiler (packageKeySupported)
#else
import Distribution.Simple.PackageIndex (PackageIndex)
#endif
import Distribution.Simple.Program
    (
      ProgramConfiguration
    , defaultProgramConfiguration
    )
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity, normal)

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
    , piSourceId    :: PackageIdentifier
    , piKey         :: String
    } deriving Eq

instance Package PkgId where
    packageId = piSourceId

instance Show PkgId where
    showsPrec _ = showsPackageId . piSourceId

instance Ord PkgId where
    compare = comparing piSourceId

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
toPkgId = PkgId <$> installedPackageId <*> sourcePackageId <*> key
  where
#if MIN_VERSION_Cabal(1, 22, 0)
    key = display . packageKey
#else
    key = show . sourcePackageId
#endif

pkgPath :: PkgId -> IO FilePath
#if MIN_VERSION_Cabal(1, 22, 0)
pkgPath pkgId = do
    (compiler, _, _) <- configCompilerEx (Just GHC) Nothing Nothing
                        defaultProgramConfiguration normal
    if packageKeySupported compiler
        then return $ piKey pkgId
        else return $ show pkgId
#else
pkgPath = return . show
#endif

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
