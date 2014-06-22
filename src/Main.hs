module Main where

import Control.Applicative ((<$>))
import Data.Version (showVersion)
import System.Environment (getArgs)

import CabalDelete
import CabalDelete.Types

import Paths_cabal_delete (version)

main :: IO ()
main = do
    e <- parseOpts <$> getArgs
    case e of
        Left u            -> usage u
        Right (cfg, pkgs) ->
            case cdCmd cfg of
                CmdVersion -> putStrLn $ "cabal-delete " ++ showVersion version
                _          -> cdMain cfg pkgs
