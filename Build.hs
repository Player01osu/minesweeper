#!/bin/runghc
import Script
import System.Environment
import System.Process
import System.Exit
import Text.Printf
import Data.List

targetDir :: String
targetDir = "target"

objDir :: String
objDir = "obj"

srcDir :: String
srcDir = "src"

src :: IO [String]
src = outProcessLines "find" [srcDir, "-type", "f"]

obj :: IO [String]
obj = outProcessLines "find" [objDir, "-type", "f"]

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

cflags :: [String]
cflags = ["-Wall", "-Wpedantic", "-std=c99"]

linkingFlags :: [String]
linkingFlags = ["-I/usr/include/SDL2", "-D_REENTRANT", "-L/usr/lib", "-lSDL2"]

makeDir :: IO ()
makeDir = makeTarget >>= \_ -> makeObj

compileSrc :: IO ()
compileSrc = do
   makeDir
   src >>= generateObj
   src >>= \x -> ((createProcess $ compileCommand x)
       >>= \(_, _, _, x) -> waitForProcess x)
   return ()


parseSrcToObj :: String -> String
parseSrcToObj src
   | srcDir `isPrefixOf` src = obj
   | otherwise = undefined
   where Just obj = stripPrefix srcDir obj
--parseSrcToObj src = printf "%s/%s" objDir $ takeWhile (/= '.') $ tail $ dropWhile (/= '/') src

generateObj :: [String] -> IO ()
--generateObj src = map (\x -> (createProcess (proc cc (["-o"] <> [parseSrcToObj x] <> [x]))) >>= \_ -> return ()))
generateObj = undefined

compileCommand :: [String] -> CreateProcess
compileCommand = proc cc . compileArgs

compileArgs :: [String] -> [String]
compileArgs obj = ["-o", targetPath] <> obj <> cflags <> linkingFlags

runBinary :: IO ()
runBinary = runCommand targetPath >>= (\_ -> return ())

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc
parseArgs ["build"] = compileSrc
parseArgs ["run"] = compileSrc >>= (\_ -> runBinary)

main :: IO ()
main = getArgs >>= parseArgs
