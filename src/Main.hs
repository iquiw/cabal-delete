module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit

import CabalDelete.Command
import CabalDelete.Types

data CDCmd = CmdList | CmdListMinor | CmdNoDeps | CmdCheck
    deriving (Eq, Show)

options :: [OptDescr CDCmd]
options =
    [ Option "l" ["multiple-versions"] (NoArg CmdList)
      "show list of packages that have multiple versions"
    , Option "m" ["multiple-minors"] (NoArg CmdListMinor)
      "show list of packages that have multiple minor versions"
    , Option "r" ["reverse-depends"] (NoArg CmdNoDeps)
      "show list of packages that have no reverse dependency"
    , Option "n" ["dry-run"] (NoArg CmdCheck)
      "check what will happen without actual action"
    ]

usage :: String -> IO ()
usage s = do
    let shortUsage = "usage: cabal-delete [options] [packages]"
    putStrLn $ usageInfo (unlines [shortUsage, s]) options
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    (opts, pkgs ,errs) <- getOpt RequireOrder options `fmap` getArgs
    case errs of
        [] -> case opts of
            [] | null pkgs -> usage "specify option or package names"
               | otherwise -> cmdDelete pkgs
            [cmd]          -> doCmd cmd pkgs 
            _              -> usage "specify exactly one option"
        _  -> usage $ concat errs
  where
    doCmd CmdList _      = cmdList (==.)
    doCmd CmdListMinor _ = cmdList (.==)
    doCmd CmdNoDeps _    = cmdNoDeps
    doCmd CmdCheck []    = usage "specify package names"
    doCmd CmdCheck pkgs  = cmdCheck pkgs
