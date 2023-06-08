module Script (outProcess, outProcessLines, sh, runProcess, voidProcess) where

import System.Process hiding (runProcess)
import System.Exit
import System.IO
import Text.Printf
import Control.Monad

type ProcessName = String
type Arguments = [String]

sh :: String -> IO ()
sh command = (voidProcess $ shell command)

printProc :: CreateProcess -> IO ()
printProc proc = putStrLn $ stringProcess (cmdspec proc)
   where
      stringProcess :: CmdSpec -> String
      stringProcess (ShellCommand command) = command
      stringProcess (RawCommand filePath command) = printf "%s %s" filePath $ unwords command

runProcess :: ProcessName -> Arguments -> IO ()
runProcess process arguments = (voidProcess $ proc process arguments)

exitOnFail :: IO ExitCode -> IO ()
exitOnFail ioExitCode = ioExitCode >>= checkExitCode
   where
      checkExitCode ExitSuccess = return ()
      checkExitCode fail        = exitWith fail

voidProcess :: CreateProcess -> IO ()
voidProcess process = printProc process >> (exitOnFail $ (createProcess $ process)
      >>= \(_, _, _, x) -> waitForProcess x)

outProcess :: ProcessName -> Arguments -> IO String
outProcess name arguments = do
   (_, Just hout, _, handle) <-
      createProcess (proc name arguments) { std_out = CreatePipe }
   printProc (proc name arguments)
   exitOnFail $ waitForProcess handle
   hGetContents hout

outProcessLines :: ProcessName -> Arguments -> IO [String]
outProcessLines name arguments = do
   result <- outProcess name arguments
   return $ lines $ result

