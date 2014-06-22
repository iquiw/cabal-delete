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
    { cdCmd       = CmdDelete
    , cdDryRun    = False
    , cdGhcLibdir = GHC.Paths.libdir
    , cdScope     = ScopeUser
    , cdMinorOnly = False
    , cdRecursive = False
    , cdYesToAll  = False
    }

options :: [OptDescr (CDConfig -> Maybe CDConfig)]
options =
    [ Option "R" ["recursive"]
      (NoArg $ \cfg -> return $ cfg { cdRecursive = True })
      "delete packages recuresively"

    , Option "a" ["all"]
      (NoArg $ \cfg -> return $ cfg { cdScope = ScopeAll })
      "process both user and global packages"

    , Option "g" ["global"]
      (NoArg $ \cfg -> return $ cfg { cdScope = ScopeGlobal })
      "process global packages instead of local packages"

    , Option "h" ["help"]
      (NoArg $ \cfg -> return $ cfg { cdCmd = CmdHelp })
      "show this help"

    , Option "i" ["info"]
      (NoArg $ nodup CmdInfo)
      "show package info"

    , Option "l" ["multiple-versions"]
      (NoArg $ nodup CmdList)
      "list packages with multiple versions"

    , Option "m" ["multiple-minors"]
      (NoArg $ \cfg -> nodup CmdList $ cfg { cdMinorOnly = True })
      "list packages with multiple minor versions"

    , Option "r" ["reverse-depends"]
      (NoArg $ nodup CmdNoDeps)
      "list packages with no reverse dependency"

    , Option "n" ["dry-run"]
      (NoArg $ \cfg -> return $ cfg { cdDryRun = True  })
      "check what will happen without actual action"

    , Option "v" ["version"]
      (NoArg $ nodup CmdVersion)
      "show version number"
    ]
  where
    nodup ncmd cfg = case cdCmd cfg of
        CmdDelete -> Just $ cfg { cdCmd = ncmd }
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
    case (cdCmd cfg, pkgs) of
        (CmdDelete, []) -> usage "specify package name"
        (CmdDelete, _)  -> withRevDepends (cdScope cfg) $ runCDM (cmdDelete pkgs) cfg
        (CmdInfo, [])   -> usage "specify package name"
        (CmdInfo, _)    -> withRevDepends (cdScope cfg) $ runCDM (cmdInfo pkgs) cfg
        (CmdList, _)    -> runCDM (cmdList pkgs) cfg
        (CmdNoDeps, _)  -> withRevDepends (cdScope cfg) $ runCDM (cmdNoDeps pkgs) cfg
        _               -> usage ""
