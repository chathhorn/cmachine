module CMachine.CLI (cli, CliSettings (..), eval) where

import System.IO
import System.Console.Haskeline 
import Control.Monad
import qualified Control.Exception as E
import Control.Monad.Trans

import Data.Char
import Data.List

import Text.Parsec

import CMachine
import CMachine.Syntax
import CMachine.Parsing 

eval :: CMaParser [Cmd] -> String -> CMaState CMa -> (CMaState CMa, Cmd)
eval parser line st = 
      case runCMachine (runParserT parser () "" line) st of
            (st', Just (Right (c:[]))) -> (st', c)
            (st', Just (Right cs)) -> (st', LoadFileResult cs)
            (st', _) -> (st', ParseFailed)

-- Special cases for tab completion.
completer :: String -> String -> IO [Completion]
completer pre str = case reverse $ trimStart pre of
      ":load" -> listFiles str
      ":pushi" -> return $ map simpleCompletion $ filter (str `isPrefixOf`) cmasList
      ":clear" -> return $ map simpleCompletion $ filter (str `isPrefixOf`) ["code", "data", "watches"]
      ":toggle" -> return $ map simpleCompletion $ filter (str `isPrefixOf`) ["code", "data", "stack"]
      "" -> return $ map simpleCompletion $ filter (str `isPrefixOf`) (cmasList ++ cmdsList)
      _ -> return []

-- Line editing (Haskeline) settings.
lineSettings :: Settings IO
lineSettings = Settings 
      { complete = completeWordWithPrev Nothing " \t" completer
      , historyFile = Nothing
      , autoAddHistory = True
      }

trimStart, trimEnd :: String -> String
trimStart = dropWhile isSpace
trimEnd = reverse . trimStart . reverse

data CliSettings = CliSettings
      { state :: CMaState CMa
      , showStack :: Bool
      , showData :: Bool
      , showCode :: Bool
      , watches :: [(Int, Int)]
      }

showWatches :: CMaState CMa -> [(Int, Int)] -> InputT IO ()
showWatches _ [] = return ()
showWatches s ((i, n):sws) = do
      outputStrLn $ "@" ++ show i ++ ": " ++ show (dumpDataSwatch s i n)
      showWatches s sws

-- TODO negative size

cli :: CliSettings -> IO ()
cli settings  = 
      runInputT lineSettings $ loop settings where 

      loop :: CliSettings -> InputT IO ()
      loop settings = do 
            outputStrLn $ showRegs $ state settings
            when (showStack settings) $
                  outputStrLn $ show (dumpStack $ state settings)
            when (showData settings) $
                  outputStrLn $ show (dumpData $ state settings)
            when (showCode settings) $
                  outputStrLn $ show (dumpCode $ state settings)
            showWatches (state settings) $ watches settings
            line <- getInputLine "> "
            case line of
                  Just line -> runFragment cliParser line settings
                  Nothing -> loop settings

      runFragment :: CMaParser [Cmd] -> String -> CliSettings -> InputT IO ()
      runFragment parser line settings = 
            let (st', r) = eval parser line $ state settings in
            case r of
                  ParseFailed -> do
                        outputStrLn "*** What?"
                        loop settings {state = st'}
                  InsLoad -> do
                        outputStrLn "*** Loading instruction."
                        loop settings {state = st'}
                  LoadAndStep -> do
                        outputStrLn "*** Loading and executing instruction."
                        loop settings {state = st'}
                  Clear -> do
                        outputStrLn "*** Clearing code and data segments."
                        loop settings {state = clear st', watches = []}
                  ClearCode -> do
                        outputStrLn "*** Clearing code segment and registers."
                        loop settings {state = clearCode st'}
                  ClearData -> do
                        outputStrLn "*** Clearing data segment and registers."
                        loop settings {state = clearData st'}
                  ClearWatches -> do
                        outputStrLn "*** Clearing watches."
                        loop settings {watches = []}
                  ResizeData sz -> do
                        outputStrLn $ "*** Resizing the data segment to " ++ show sz ++ " cells."
                        loop settings {state = resizeData sz st'}
                  Step -> do
                        outputStrLn "*** Fetching and executing next instruction."
                        loop settings {state = st'}
                  Exec -> do
                        outputStrLn "*** Executing the loaded program (ctrl-c to abort)."
                        e <- lift (E.try $ E.evaluate $ runCMachine execute st') 
                        case e of
                              Right (st'', _) -> loop settings {state = st''}
                              Left E.UserInterrupt -> do
                                    outputStrLn "\n*** Aborted."
                                    loop settings {state = st'}
                  LoadFile fp -> do
                        outputStrLn $ "*** Loading file: " ++ fp
                        contents <- lift $ readFile $ trimEnd fp
                        runFragment fileParser contents $ settings {state = st'}
                  ToggleStack -> do
                        outputStrLn "*** Toggling the stack display."
                        loop settings {showStack = not $ showStack settings}
                  ToggleData -> do
                        outputStrLn "*** Toggling the data segment display."
                        loop settings {showData = not $ showData settings}
                  ToggleCode -> do
                        outputStrLn "*** Toggling the code segment display."
                        loop settings {showCode = not $ showCode settings}
                  Watch n i -> do
                        outputStrLn $ "*** Adding a watch at offset " ++ show i ++ "."
                        loop settings {watches = watches settings ++ [(i, n)]}
                  Help -> do
                        outputStrLn "*** Help you?"
                        loop settings
                  Quit -> return ()
                  LoadFileResult _ ->
                        loop settings {state = st'}

