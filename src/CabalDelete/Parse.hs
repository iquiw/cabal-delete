{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module CabalDelete.Parse
    ( parsePkgId
    , parseGhcPkgList
    ) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import Distribution.Package
import Text.Parsec

import CabalDelete.Types

type P u a = (Stream s m Char) => ParsecT s u m a

parseGhcPkgList :: String -> Either ParseError PkgConfList
parseGhcPkgList = runParser (many (try _warnMsg) *> _ghcPkgList) Nothing ""

_warnMsg :: P u ()
_warnMsg = string "WARNING" *> many (noneOf "\n") *> _eol

_ghcPkgList :: P (Maybe Version) PkgConfList
_ghcPkgList = many $ liftM2 (,) _pkgConfPath _pkgList

_pkgConfPath :: P (Maybe Version) FilePath
_pkgConfPath = anyChar `manyTill` try (char ':' *> _eol)

_pkgList :: P (Maybe Version) [PackageId]
_pkgList = concat <$> many (_pkgLine <* _eol) <* optional _eol

_pkgLine :: P (Maybe Version) [PackageId]
_pkgLine = many1 (char ' ') *> sepBy p sep
  where
    p = optional (oneOf "({") *> _pkgId <* optional (oneOf ")}")
    sep = char ',' *> spaces

_eol :: P u ()
_eol = () <$ (optional (char '\r') >> char '\n')

parsePkgId :: String -> Either ParseError PackageId
parsePkgId = runParser (_pkgId <* eof) Nothing ""

_pkgId :: P (Maybe Version) PackageId
_pkgId = do
    cs <- _nameChunk `manyTill` (try _numVer >>= setState . Just)
    v <- fromJust <$> getState
    let name = PackageName $ intercalate "-" cs
    return $ PackageIdentifier name v

_nameChunk :: P u String
_nameChunk = anyChar `manyTill` char '-'

_numVer :: P u Version
_numVer = (flip Version [] . map read) <$> sepBy1 (many1 digit) (char '.')
