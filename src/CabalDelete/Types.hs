{-# LANGUAGE CPP #-}
module CabalDelete.Types 
    ( PackageEq
    , PackageInfo
    , PkgConfList
    , PkgId
    , (==.)
    , (.==)
    , getPackages
    , getPkgConfs
    , toPkgId
    , pkgKey
    , showsPackageId
    ) where

import Control.Monad
import Data.Version
import Distribution.Package
import Distribution.InstalledPackageInfo
import System.FilePath ((</>))

type PackageInfo = InstalledPackageInfo_ String

type PkgConfList = [(FilePath, [PackageId])]

type PackageEq = PackageId -> PackageId -> Bool

getPackages :: PkgConfList -> [PackageId]
getPackages = concatMap snd

(.==) :: PackageEq
(.==) (PackageIdentifier (PackageName n1) (Version (v1:_) _))
      (PackageIdentifier (PackageName n2) (Version (v2:_) _))
    = n1 == n2 && v1 == v2
(.==) _ _ = False

(==.) :: PackageEq
(==.) (PackageIdentifier (PackageName n1) _)
      (PackageIdentifier (PackageName n2) _)
    = n1 == n2

showsPackageId :: PackageId -> ShowS
showsPackageId (PackageIdentifier (PackageName n) v) =
    (n ++) . ("-" ++) . (showVersion v ++)

#if __GLASGOW_HASKELL__ >= 612
data PkgId = PkgId InstalledPackageId PackageId deriving (Eq, Ord)

instance Package PkgId where
    packageId (PkgId _ i) = i

instance Show PkgId where
    showsPrec _ (PkgId _ i) = showsPackageId i
  
toPkgId :: PackageInfo -> PkgId
toPkgId = liftM2 PkgId installedPackageId sourcePackageId

pkgKey :: PkgId -> InstalledPackageId
pkgKey (PkgId ipid _) = ipid

getPkgConfs :: PkgConfList -> [FilePath]
getPkgConfs = map ((</> "package.cache") . fst)

#else
newtype PkgId = PkgId PackageId deriving (Eq, Ord)

instance Package PkgId where
    packageId (PkgId i) = i

instance Show PkgId where
    showsPrec _ (PkgId i) = showsPackageId i

toPkgId :: PackageInfo -> PkgId
toPkgId = PkgId . package

pkgKey :: PkgId -> PackageId
pkgKey (PkgId i) = i

getPkgConfs :: PkgConfList -> [FilePath]
getPkgConfs = map fst
#endif
