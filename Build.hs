#!/bin/runghc
import Script
import System.Environment
import System.Process hiding (runProcess)
import System.Exit
import Control.Monad
import Text.Printf
import Data.List

targetDir :: String
targetDir = "target"

objDir :: String
objDir = "obj"

srcDir :: String
srcDir = "src"

src :: IO [String]
src = outProcessLines "find" [srcDir, "-type", "f", "-name", "*.c"]

obj :: IO [String]
obj = outProcessLines "find" [objDir, "-type", "f"]

srcs :: IO [String]
srcs = do
   src <- src
   objs <- obj
   obj <- (foldMap
      (\file -> outProcessLines "find" $ [file] <> ["-cnewer"] <> [parseObjToSrc file])
      objs)
   return $ src \\ map parseObjToSrc obj

makeTarget :: IO ()
makeTarget = sh $ printf "mkdir -p %s" targetDir

makeObj :: IO ()
makeObj = sh $ printf "mkdir -p %s" objDir

binName :: String
binName = "minesweeper"

targetPath :: String
targetPath = printf "%s/%s" targetDir binName

cc :: String
cc = "clang"

cFlags :: [String]
cFlags = ["-Wall", "-Wpedantic", "-std=c99", "-Wextra", "-fno-omit-frame-pointer", "-g3"]
-- "-fsanitize=address"

linkingFlags :: [String]
linkingFlags = ["-I/usr/include/SDL2", "-D_REENTRANT", "-L/usr/lib", "-lSDL2", "-lSDL2_ttf"]

makeDir :: IO ()
makeDir = makeTarget >> makeObj

compileSrc :: IO ()
compileSrc = do
   makeDir
   srcs >>= generateObj
   obj >>= voidProcess . compileCommand

parseObjToSrc :: String -> String
parseObjToSrc obj
   | objDir `isPrefixOf` obj = printf "%s%s.c" srcDir (takeWhile (/= '.') src)
   | otherwise = undefined
   where Just src = stripPrefix objDir obj

parseSrcToObj :: String -> String
parseSrcToObj src
   | srcDir `isPrefixOf` src = printf "%s%s.o" objDir (takeWhile (/= '.') obj)
   | otherwise = undefined
   where Just obj = stripPrefix srcDir src

generateObj :: [String] -> IO ()
generateObj =
   foldMap (\x -> runProcess cc (["-o"] <> [parseSrcToObj x] <> ["-c"] <> [x]))

compileCommand :: [String] -> CreateProcess
compileCommand = proc cc . compileArgs

compileArgs :: [String] -> [String]
compileArgs obj = obj <> cFlags <> linkingFlags <> ["-o", targetPath]

runBinary :: IO ()
runBinary = void $ runCommand targetPath

cleanWorkingDir :: IO ()
cleanWorkingDir = runProcess "rm" ["-rf", targetDir, objDir]

compileBuild :: IO ()
compileBuild = runProcess "ghc" ["-no-keep-hi-files", "-no-keep-o-files", "Build.hs"]

invalidArg :: [String] -> IO ()
invalidArg = putStrLn . printf "Invalid argument: %s" . unwords

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc
parseArgs args
   | args `elem` [["build"], ["b"]]          = compileSrc
   | args `elem` [["run"], ["r"]]            = compileSrc >> runBinary
   | args `elem` [["clean"], ["c"]]          = cleanWorkingDir
   | args `elem` [["compile-build"], ["cb"]] = compileBuild
   | args `elem` [["force"], ["f"]]          = cleanWorkingDir >> compileSrc
   | args `elem` [["force-run"], ["fr"]]     = cleanWorkingDir >> compileSrc >> runBinary
   | otherwise                               = invalidArg args

main :: IO ()
main = getArgs >>= parseArgs
