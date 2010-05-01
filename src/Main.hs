module Main where

import Data.Char
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import CabalDelete.Command

import Paths_cabal_delete (version)

data CDCmd =
      CmdHelp
    | CmdInfo
    | CmdList
    | CmdListMinor
    | CmdNoDeps
    | CmdCheck
    | CmdVersion
    deriving (Eq, Show)

options :: [OptDescr CDCmd]
options =
    [ Option "h" ["help"] (NoArg CmdHelp)
      "show this help"
    , Option "i" ["info"] (NoArg CmdInfo)
      "show package info"
    , Option "l" ["multiple-versions"] (NoArg CmdList)
      "list packages with multiple versions"
    , Option "m" ["multiple-minors"] (NoArg CmdListMinor)
      "list packages with multiple minor versions"
    , Option "r" ["reverse-depends"] (NoArg CmdNoDeps)
      "list packages with no reverse dependency"
    , Option "n" ["dry-run"] (NoArg CmdCheck)
      "check what will happen without actual action"
    , Option "v" ["version"] (NoArg CmdVersion)
      "show version number"
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
    doCmd CmdInfo []     = usage "specify package names"
    doCmd CmdInfo pkgs   = cmdInfo pkgs
    doCmd CmdList _      = cmdList
    doCmd CmdListMinor _ = cmdListMinor
    doCmd CmdNoDeps _    = cmdNoDeps
    doCmd CmdCheck []    = usage "specify package names"
    doCmd CmdCheck pkgs  = cmdCheck pkgs
    doCmd CmdVersion _   = putStrLn $ "cabal-delete " ++ showVersion version

    chop = reverse . dropWhile isSpace . reverse
