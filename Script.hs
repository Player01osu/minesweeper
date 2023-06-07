module Script (outProcess, outProcessLines, sh) where

import System.Process
import System.IO

type ProcessName = String
type Arguments = [String]

sh :: String -> IO ()
sh command = do
   runCommand command >>= \x -> waitForProcess x
   return ()

outProcess :: ProcessName -> Arguments -> IO String
outProcess name arguments = do
   (_, Just hout, _, _) <-
      createProcess (proc name arguments) { std_out = CreatePipe }
   hGetContents hout

outProcessLines :: ProcessName -> Arguments -> IO [String]
outProcessLines name arguments = do
   result <- outProcess name arguments
   return $ lines $ result

