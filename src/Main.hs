module Main where

import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import CabalDelete.Command
import CabalDelete.Types

data CDCmd =
      CmdHelp
    | CmdList
    | CmdListMinor
    | CmdNoDeps
    | CmdCheck
    deriving (Eq, Show)

options :: [OptDescr CDCmd]
options =
    [ Option "h" ["help"] (NoArg CmdHelp)
      "show this help"
    , Option "l" ["multiple-versions"] (NoArg CmdList)
      "show list of packages that have multiple versions"
    , Option "m" ["multiple-minors"] (NoArg CmdListMinor)
      "show list of packages that have multiple minor versions"
    , Option "r" ["reverse-depends"] (NoArg CmdNoDeps)
      "show list of packages that have no reverse dependency"
    , Option "n" ["dry-run"] (NoArg CmdCheck)
      "check what will happen without actual action"
    ]

usage :: String -> IO ()
usage err = do
    let us = "usage: cabal-delete [options] [packages]"
        (msg, out, ecode) = if null err
                            then (unlines [us], stdout, ExitSuccess)
                            else (unlines [us, err], stderr, ExitFailure 1)
    hPutStrLn out $ usageInfo msg options
    exitWith ecode

main :: IO ()
main = do
    (opts, pkgs ,errs) <- getOpt RequireOrder options `fmap` getArgs
    case errs of
        [] -> case opts of
            [] | null pkgs           -> usage "specify option or package names"
               | otherwise           -> cmdDelete pkgs
            [cmd]                    -> doCmd cmd pkgs 
            _  | CmdHelp `elem` opts -> doCmd CmdHelp []
               | otherwise           -> usage "specify exactly one option"
        _  -> usage $ chop $ concat errs
  where
    doCmd CmdHelp _      = usage []
    doCmd CmdList _      = cmdList (==.) 
    doCmd CmdListMinor _ = cmdList (.==)
    doCmd CmdNoDeps _    = cmdNoDeps
    doCmd CmdCheck []    = usage "specify package names"
    doCmd CmdCheck pkgs  = cmdCheck pkgs

    chop = reverse . dropWhile isSpace . reverse
