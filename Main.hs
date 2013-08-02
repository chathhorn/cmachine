module Main( main ) where

import System.Exit (exitSuccess)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Data.Maybe( fromMaybe )

import CMachine
import CMachine.CLI (cli, CliSettings (..), eval)
import CMachine.Parsing (fileParser)

defInitialSize = 50

defOptions :: Options
defOptions = Options 
      { fileContents = return ""
      , gotFile = False
      , size = defInitialSize
      }

getCliSettings :: Options -> CliSettings
getCliSettings opts = CliSettings 
      { state = initCMaState $ size opts
      , showStack = True
      , showData = True
      , showCode = True
      , watches = []
      }

main = do
      args <- getArgs

      let (actions, nonOpts, msgs) = getOpt (ReturnInOrder setFileContents) options args

      opts <- foldl (>>=) (return defOptions) actions

      if gotFile opts
            then do
                  contents <- fileContents opts
                  let (st, cmd) = eval fileParser contents (initCMaState $ size opts)
                  let (st', _) = runCMachine execute st
                  print st'
            else cli $ getCliSettings opts

data Options = Options  
      { fileContents  :: IO String
      , gotFile :: Bool
      , size :: Int
      }

options :: [OptDescr (Options -> IO Options)]
options = 
      [ Option "h" ["help"]    (NoArg showUsage)                "show usage information"
      --, Option ['f'] ["file"]    (ReqArg setFileContents "FILE")  "input file to run"
      , Option "s" ["size"]    (ReqArg setSize "SIZE")          "initial size of the VM's data segment"
      ]

showUsage _ = do
      exec <- getProgName
      let header = "Usage: " ++ exec ++ " [options] [FILE]\n" ++
            "Load and run CMa instructions from FILE or, if FILE omitted,\n" ++
            "start an interactive command-line interpreter."
      putStrLn $ usageInfo header options
      exitSuccess
      
setFileContents fp opt = return opt { fileContents = readFile fp, gotFile = True }
setSize sz opt = return opt { size = read sz }

