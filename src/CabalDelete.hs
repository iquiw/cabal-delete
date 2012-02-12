module CabalDelete
    ( cdMain
    , parseOpts
    , usage ) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import qualified GHC.Paths (libdir)
import System.Console.GetOpt
    (ArgDescr(..), ArgOrder(..), OptDescr(..), usageInfo, getOpt)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr, stdout)

import CabalDelete.Command
import CabalDelete.ReverseDepends
import CabalDelete.Types

defaultCfg :: CDConfig
defaultCfg = CDConfig
    { cmd       = CmdDelete
    , dryRun    = False
    , ghcLibdir = GHC.Paths.libdir
    , minorOnly = False
    , recursive = False
    , yesToAll  = False
    }

options :: [OptDescr (CDConfig -> Maybe CDConfig)]
options =
    [ Option "R" ["recursive"]
      (NoArg $ \cfg -> return $ cfg { recursive = True })
      "delete packages recuresively"

    , Option "h" ["help"]
      (NoArg $ \cfg -> return $ cfg { cmd = CmdHelp })
      "show this help"

    , Option "i" ["info"]
      (NoArg $ nodup CmdInfo)
      "show package info"

    , Option "l" ["multiple-versions"]
      (NoArg $ nodup CmdList)
      "list packages with multiple versions"

    , Option "m" ["multiple-minors"]
      (NoArg $ \cfg -> nodup CmdList $ cfg { minorOnly = True })
      "list packages with multiple minor versions"

    , Option "r" ["reverse-depends"]
      (NoArg $ nodup CmdNoDeps)
      "list packages with no reverse dependency"

    , Option "n" ["dry-run"]
      (NoArg $ \cfg -> return $ cfg { dryRun = True  })
      "check what will happen without actual action"

    , Option "v" ["version"]
      (NoArg $ nodup CmdVersion)
      "show version number"
    ]
  where
    nodup ncmd cfg = case cmd cfg of
        CmdDelete -> Just $ cfg { cmd = ncmd }
        _         -> Nothing

usage :: String -> IO a
usage err = do
    let us = "usage: cabal-delete [option] [package..]"
        (msg, out, ecode) = if null err
                            then (unlines [us], stdout, ExitSuccess)
                            else (unlines [us, err], stderr, ExitFailure 1)
    hPutStrLn out $ usageInfo msg options
    exitWith ecode

parseOpts :: [String] -> Either String (CDConfig, [String])
parseOpts args =
    let (fs, pkgs ,errs) = getOpt Permute options args
    in case errs of
        [] -> case foldM (flip id) defaultCfg fs of
            Nothing  -> Left ""
            Just cfg -> Right (cfg, pkgs)
        _  -> Left $ chop $ concat errs
  where
    chop = reverse . dropWhile isSpace . reverse

cdMain :: CDConfig -> [String] -> IO ()
cdMain cfg pkgs =
    case (cmd cfg, pkgs) of
        (CmdDelete, []) -> usage "specify package name"
        (CmdDelete, _)  -> withRevDepends $ runCDM (cmdDelete pkgs) cfg
        (CmdInfo, [])   -> usage "specify package name"
        (CmdInfo, _)    -> withRevDepends $ runCDM (cmdInfo pkgs) cfg
        (CmdList, _)    -> runCDM (cmdList pkgs) cfg
        (CmdNoDeps, _)  -> withRevDepends $ runCDM (cmdNoDeps pkgs) cfg
        _               -> usage ""
