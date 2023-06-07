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
cFlags = ["-Wall", "-Wpedantic", "-std=c99"]

linkingFlags :: [String]
linkingFlags = ["-I/usr/include/SDL2", "-D_REENTRANT", "-L/usr/lib", "-lSDL2"]

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

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc
parseArgs ["build"] = compileSrc
parseArgs ["run"] = compileSrc >> runBinary

main :: IO ()
main = getArgs >>= parseArgs
