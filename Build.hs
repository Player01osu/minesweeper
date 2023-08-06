#!/bin/runghc
import Script
import System.Environment
import System.Process hiding (runProcess)
import System.Exit
import Control.Monad
import Control.Exception
import Text.Printf
import Data.List
import Data.Either

targetDir :: String
targetDir = "target"

objDir :: String
objDir = "obj"

srcDir :: String
srcDir = "src"

src :: IO [String]
src = outProcessLines "find" [srcDir, "-type", "f", "-name", "*.c"]

obj :: String -> IO [String]
obj objDir = outProcessLines "find" [objDir, "-type", "f"]

srcs :: String -> IO [String]
srcs objDir = do
   src <- src
   objs <- obj objDir
   obj <- (foldMap
      --(\file -> outProcessLines "find" $ [file] <> ["-cnewer"] <> [parseObjToSrc file objDir])
      (\file -> outProcessLines "find" $ [objDir] <> ["-cnewer"] <> [parseObjToSrc file objDir])
      objs)
   return $ src \\ map (\x -> parseObjToSrc x objDir) obj

mkDir dir = sh $ printf "mkdir -p %s" dir

binName :: String
binName = "minesweeper"

targetPath :: String -> String
targetPath targetDir = printf "%s/%s" targetDir binName

cc :: String
cc = "clang"

cFlags :: [String]
cFlags = ["-Wall", "-Wpedantic", "-std=c99", "-Wextra", "-fno-omit-frame-pointer"]
-- "-fsanitize=address"

linkingFlags :: [String]
linkingFlags = ["-I/usr/include/SDL2", "-D_REENTRANT", "-L/usr/lib", "-lSDL2", "-lSDL2_ttf"]

makeDir :: String -> String -> IO ()
makeDir objDir targetDir = mkDir objDir >> mkDir targetDir

compileSrc :: [Flag] -> IO ()
compileSrc flag = do
   makeDir objDir targetDir
   srcs objDir >>= generateObj flags objDir
   obj objDir >>= voidProcess . compileCommand flags targetDir
   where
      flags = flags' flag
      flags' []               = ["-g3"]
      flags' (FlagDebug:xs)   = ["-g3"]
      flags' (FlagRelease:xs) = ["-O2", "-ffast-math"]

      objDir = objDir' flag
      objDir' []               = "obj/debug"
      objDir' (FlagDebug:xs)   = "obj/debug"
      objDir' (FlagRelease:xs) = "obj/release"

      targetDir = targetDir' flag
      targetDir' []               = "target/debug"
      targetDir' (FlagDebug:xs)   = "target/debug"
      targetDir' (FlagRelease:xs) = "target/release"

parseObjToSrc :: String -> String -> String
parseObjToSrc [] objDir = ""
parseObjToSrc obj objDir
   | objDir `isPrefixOf` obj = printf "%s%s.c" srcDir (takeWhile (/= '.') src)
   | otherwise = undefined
   where Just src = stripPrefix objDir obj

parseSrcToObj :: String -> String -> String
parseSrcToObj src objDir
   | srcDir `isPrefixOf` src = printf "%s%s.o" objDir (takeWhile (/= '.') obj)
   | otherwise = undefined
   where Just obj = stripPrefix srcDir src

generateObj :: [String] -> String -> [String] -> IO ()
generateObj flags objDir =
   foldMap (\x -> runProcess cc (["-o"] <> [parseSrcToObj x objDir] <> cFlags <> flags <> ["-c"] <> [x]))

compileCommand :: [String] -> String -> [String] -> CreateProcess
compileCommand flags targetDir = proc cc . (compileArgs flags targetDir)

compileArgs :: [String] -> String -> [String] -> [String]
compileArgs flags targetDir obj = obj <> flags <> cFlags <> linkingFlags <> ["-o", targetPath targetDir]

runBinary :: [Flag] -> IO ()
runBinary flags = void $ runCommand $ targetPath $ targetDir flags
   where
      targetDir []               = "target/debug"
      targetDir (FlagDebug:xs)   = "target/debug"
      targetDir (FlagRelease:xs) = "target/release"

cleanWorkingDir :: IO ()
cleanWorkingDir = runProcess "rm" ["-rf", targetDir, objDir]

compileBuild :: IO ()
compileBuild = runProcess "ghc" ["-no-keep-hi-files", "-no-keep-o-files", "Build.hs"]

data SubCmd = SubCmdBuild
            | SubCmdRun
            | SubCmdClean
            | SubCmdCompileBuild
            | SubCmdForce
            | SubCmdForceRun

data Flag = FlagDebug
          | FlagRelease

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc [FlagDebug]
parseArgs args = executeSubCommand $ pairArgs subCommand flags
   where
      executeSubCommand :: Either (SubCmd, [Flag]) String -> IO ()
      executeSubCommand (Left (SubCmdBuild,        flags)) = compileSrc flags
      executeSubCommand (Left (SubCmdRun,          flags)) = compileSrc flags >> runBinary flags
      executeSubCommand (Left (SubCmdClean,        flags)) = cleanWorkingDir
      executeSubCommand (Left (SubCmdCompileBuild, flags)) = compileBuild
      executeSubCommand (Left (SubCmdForce,        flags)) = cleanWorkingDir >> compileSrc flags
      executeSubCommand (Left (SubCmdForceRun,     flags)) = cleanWorkingDir >> compileSrc flags >> runBinary flags
      executeSubCommand (Right msg)                        = putStrLn msg >> die

      pairArgs :: Either SubCmd String -> Either [Flag] String -> Either (SubCmd, [Flag]) String
      pairArgs (Right msg) _              = Right msg
      pairArgs _ (Right msg)              = Right msg
      pairArgs (Left subCmd) (Left flags) = Left (subCmd, flags)

      matchSubCommand :: String -> Either SubCmd String
      matchSubCommand cmd
         | cmd `elem` ["build", "b"]          = Left SubCmdBuild
         | cmd `elem` ["run", "r"]            = Left SubCmdRun
         | cmd `elem` ["clean", "c"]          = Left SubCmdClean
         | cmd `elem` ["compile-build", "cb"] = Left SubCmdCompileBuild
         | cmd `elem` ["force", "f"]          = Left SubCmdForce
         | cmd `elem` ["force-run", "fr"]     = Left SubCmdForceRun
         | otherwise                          = Right $ printf "Invalid subcommand: %s" cmd

      matchFlag :: String -> Either Flag String
      matchFlag flag
         | flag `elem` ["--debug"]   = Left FlagDebug
         | flag `elem` ["--release"] = Left FlagRelease
         | otherwise                 = Right $ printf "Invalid flag: %s" flag

      rawSubCmd :: [String]
      rawFlags :: [String]
      (rawFlags, rawSubCmd) = partition (isPrefixOf "--") args

      subCommand :: Either SubCmd String
      subCommand = parseSubCommand rawSubCmd

      flags :: Either [Flag] String
      flags
         | flagErrors == "" = Left $ lefts mappedFlags
         | otherwise        = Right flagErrors

      flagErrors = unwords $ rights mappedFlags

      mappedFlags :: [Either Flag String]
      mappedFlags = map matchFlag rawFlags

      parseSubCommand :: [String] -> Either SubCmd String
      parseSubCommand [] = Left SubCmdBuild
      parseSubCommand [x] = matchSubCommand x
      parseSubCommand _ = Right "Too many subcommands provided"

      die = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= parseArgs
