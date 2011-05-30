module Main where

import System (getArgs)
import System.Console.GetOpt
import System.IO
import System.Directory

import System.FS.RDFS

data Options =
    Options
    { optVerbose      :: Bool
    , optShowVersion  :: Bool
    , optFiles        :: [FilePath]
    } deriving Show

defaultOptions =
    Options
    { optVerbose      = False
    , optShowVersion  = False
    , optFiles        = []
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"]
      (NoArg (\ opts -> opts{optVerbose = True}))
      "chatty output on stderr"

    , Option ['V','?'] ["version"]
      (NoArg (\ opts -> opts{optShowVersion = True}))
      "show version number"
    ]

cOpts :: [String] -> IO (Options, [String])
cOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
          where header = "Usage: sugarsync [OPTION...] files..."

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  (opts, fsopts) <- cOpts args
  --print $ opts
  let fuseopts = ("-f":fsopts)
  --print $ fuseopts
  rdfsRun "rdfs" fuseopts


printUsage = putStrLn usage
usage = "\nUsage: sugarsync"

printHelp = do
  putStrLn $ "Here Help"
  putStrLn $ ""
