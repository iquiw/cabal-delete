{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module CabalDelete.Parse
    ( parsePkgId
    , parseGhcPkgList
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Version
import Distribution.Package
import Data.Attoparsec (eitherResult)
import Data.Attoparsec.Char8

import CabalDelete.Types

parseGhcPkgList :: String -> Either String PkgConfList
parseGhcPkgList = eitherResult . flip feed C.empty . parse _ghcPkgList . C.pack

_ghcPkgList :: Parser  PkgConfList
_ghcPkgList = many (try _warnMsg) *> many (liftM2 (,) _pkgConfPath _pkgList)

_warnMsg :: Parser ()
_warnMsg = string (C.pack "WARNING") *> many (notChar '\n') *> _eol

_pkgConfPath :: Parser FilePath
_pkgConfPath = anyChar `manyTill` try (char ':' *> _eol)

_pkgList :: Parser [PackageId]
_pkgList = concat <$> many (_pkgLine <* _eol) <* optional _eol

_pkgLine :: Parser [PackageId]
_pkgLine = many1 (char ' ') *> sepBy p sep
  where
    p = optional (oneOf "({") *> _pkgId <* optional (oneOf ")}")
    sep = char ',' *> many space
    oneOf = choice . map char

_eol :: Parser ()
_eol = () <$ (optional (char '\r') >> char '\n')

parsePkgId :: String -> Either String PackageId
parsePkgId = eitherResult . flip feed C.empty . parse _pkgId . C.pack

_pkgId :: Parser PackageId
_pkgId = go []
  where
    go cs = do
        c <- _nameChunk
        mv <- optional _numVer
        case mv of
            Nothing -> go (c:cs)
            Just v  ->
                let name = PackageName $ intercalate "-" $ reverse (c:cs)
                in return $ PackageIdentifier name v

_nameChunk :: Parser String
_nameChunk = anyChar `manyTill` char '-'

_numVer :: Parser Version
_numVer = (flip Version [] . map read) <$> sepBy1 (many1 digit) (char '.')
