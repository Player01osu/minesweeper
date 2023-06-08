module Script (outProcess, outProcessLines, sh, runProcess, voidProcess) where

import System.Process hiding (runProcess)
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

voidProcess :: CreateProcess -> IO ()
voidProcess process = (void $ (createProcess $ process)
      >>= \(_, _, _, x) -> waitForProcess x) >> printProc process

outProcess :: ProcessName -> Arguments -> IO String
outProcess name arguments = do
   (_, Just hout, _, _) <-
      createProcess (proc name arguments) { std_out = CreatePipe }
   printProc (proc name arguments)
   hGetContents hout

outProcessLines :: ProcessName -> Arguments -> IO [String]
outProcessLines name arguments = do
   result <- outProcess name arguments
   return $ lines $ result

