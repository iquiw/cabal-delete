{-# LANGUAGE OverloadedStrings #-}
module CabalDelete.Parse
    ( parsePkgId
    , parseGhcPkgList
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.List
import           Data.Monoid (mempty)
import qualified Data.Text as T
import           Data.Version
import           Distribution.Package
import           Prelude

import           CabalDelete.Types

parseGhcPkgList :: String -> Either String PkgConfList
parseGhcPkgList = eitherResult . flip feed mempty . parse _ghcPkgList . T.pack

_ghcPkgList :: Parser PkgConfList
_ghcPkgList = many (try _warnMsg) *> many ((,) <$> _pkgConfPath <*> _pkgList)

_warnMsg :: Parser ()
_warnMsg = string "WARNING" *> many (notChar '\n') *> _eol

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
parsePkgId = eitherResult . flip feed mempty . parse _pkgId . T.pack

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
