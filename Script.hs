module Script (outProcess, outProcessLines, sh, runProcess, voidProcess) where

import System.Process hiding (runProcess)
import System.IO
import Control.Monad

type ProcessName = String
type Arguments = [String]

sh :: String -> IO ()
sh command = voidProcess $ shell command

runProcess :: ProcessName -> Arguments -> IO ()
runProcess process arguments = voidProcess $ proc process arguments

voidProcess :: CreateProcess -> IO ()
voidProcess process = void $ (createProcess $ process)
      >>= \(_, _, _, x) -> waitForProcess x

outProcess :: ProcessName -> Arguments -> IO String
outProcess name arguments = do
   (_, Just hout, _, _) <-
      createProcess (proc name arguments) { std_out = CreatePipe }
   hGetContents hout

outProcessLines :: ProcessName -> Arguments -> IO [String]
outProcessLines name arguments = do
   result <- outProcess name arguments
   return $ lines $ result

