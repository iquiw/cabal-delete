{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CabalDelete.Types 
    ( CDCmd(..)
    , CDConfig(..)
    , CDM
    , runCDM
    , PackageEq
    , PkgConfList
    , PkgId(..)
    , (=-=)
    , (==.)
    , (.==)
    , toPkgId
    , showsPackageId
    ) where

import Control.Monad (liftM2)
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

data CDCmd
    = CmdHelp
    | CmdInfo
    | CmdList
    | CmdDelete
    | CmdNoDeps
    | CmdVersion
    deriving (Eq, Show, Ord)

data CDConfig = CDConfig
    { cmd       :: CDCmd
    , dryRun    :: Bool
    , minorOnly :: Bool
    , recursive :: Bool
    , yesToAll  :: Bool
    }

type CDM m = StateT CDConfig m

runCDM :: (Monad m) => CDM m a -> CDConfig -> m a
runCDM = evalStateT

type PkgConfList = [(FilePath, [PackageId])]

type PackageEq = PackageId -> PackageId -> Bool

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

instance EqIC InstalledPackageId where
    (=-=) (InstalledPackageId i1) (InstalledPackageId i2) = i1 =-= i2

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
(.==) :: PackageEq
(.==) (PackageIdentifier (PackageName n1) (Version (v1:v1':_) _))
      (PackageIdentifier (PackageName n2) (Version (v2:v2':_) _))
    = n1 =-= n2 && v1 == v2 && v1' == v2'
(.==) _ _ = False

-- | returns True if packages' name are same.
(==.) :: PackageEq
(==.) (PackageIdentifier (PackageName n1) _)
      (PackageIdentifier (PackageName n2) _)
    = n1 =-= n2

showsPackageId :: PackageId -> ShowS
showsPackageId (PackageIdentifier (PackageName n) v) =
    (n ++) . ("-" ++) . (showVersion v ++)

toPkgId :: InstalledPackageInfo -> PkgId
toPkgId = liftM2 PkgId installedPackageId sourcePackageId
