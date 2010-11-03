{-# LANGUAGE CPP #-}
module CabalDelete.Types 
    ( PackageEq
    , PackageInfo
    , PkgConfList
    , PkgId
    , PkgKey
    , (=-=)
    , (==.)
    , (.==)
    , getPackages
    , getPkgConfs
    , toPkgId
    , pkgKey
    , showsPackageId
    ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.Version
import Distribution.Package
import Distribution.InstalledPackageInfo
import System.FilePath ((</>))

type PackageInfo = InstalledPackageInfo_ String

type PkgConfList = [(FilePath, [PackageId])]

type PackageEq = PackageId -> PackageId -> Bool

class EqIC a where
    (=-=) :: a -> a -> Bool

instance EqIC Char where
    (=-=) = (==) `on` toLower

instance EqIC a => EqIC [a] where
    (=-=) = (and .) . zipWith (=-=)

instance EqIC PackageName where
    (=-=) (PackageName n1) (PackageName n2) = n1 =-= n2

getPackages :: PkgConfList -> [PackageId]
getPackages = concatMap snd

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

#if __GLASGOW_HASKELL__ >= 612
data PkgId = PkgId InstalledPackageId PackageId deriving Eq

instance Package PkgId where
    packageId (PkgId _ i) = i

instance Show PkgId where
    showsPrec _ (PkgId _ i) = showsPackageId i

instance Ord PkgId where
    compare (PkgId _ i1) (PkgId _ i2) = compare i1 i2
  
type PkgKey = InstalledPackageId

instance EqIC PackageIdentifier where
    (=-=) (PackageIdentifier n1 v1) (PackageIdentifier n2 v2) =
        n1 =-= n2 && v1 == v2

instance EqIC InstalledPackageId where
    (=-=) (InstalledPackageId i1) (InstalledPackageId i2) = i1 =-= i2

toPkgId :: PackageInfo -> PkgId
toPkgId = liftM2 PkgId installedPackageId sourcePackageId

pkgKey :: PkgId -> PkgKey
pkgKey (PkgId ipid _) = ipid

getPkgConfs :: PkgConfList -> [FilePath]
getPkgConfs = map ((</> "package.cache") . fst)

#else
newtype PkgId = PkgId PackageId deriving (Eq, Ord)

instance Package PkgId where
    packageId (PkgId i) = i

instance Show PkgId where
    showsPrec _ (PkgId i) = showsPackageId i

type PkgKey = PackageId

instance EqIC PackageIdentifier where
    (=-=) (PackageIdentifier n1 v1) (PackageIdentifier n2 v2) =
        n1 =-= n2 && v1 == v2

toPkgId :: PackageInfo -> PkgId
toPkgId = PkgId . package

pkgKey :: PkgId -> PkgKey
pkgKey (PkgId i) = i

getPkgConfs :: PkgConfList -> [FilePath]
getPkgConfs = map fst
#endif
