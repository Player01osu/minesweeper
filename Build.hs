#!/bin/runghc
import Control.Monad (void)
import Data.Either
import Data.List
import Script
import System.Environment
import System.Exit
import System.Posix.Files (fileExist, getFileStatus, modificationTime)
import System.Process hiding (runProcess)
import Text.Printf (printf)

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

mkDir dir = sh $ printf "mkdir -p %s" dir

binName :: String
binName = "minesweeper"

targetPath :: String -> String
targetPath targetDir = printf "%s/%s" targetDir binName

cc :: String
cc = "clang"

cFlags :: [String]
cFlags =
  ["-Wall", "-Wpedantic", "-std=c99", "-Wextra", "-fno-omit-frame-pointer"]

-- "-fsanitize=address"
linkingFlags :: [String]
linkingFlags =
  ["-I/usr/include/SDL2", "-D_REENTRANT", "-L/usr/lib", "-lSDL2", "-lSDL2_ttf"]

makeDir :: String -> String -> IO ()
makeDir objDir targetDir = mkDir objDir >> mkDir targetDir

compileSrc :: [Flag] -> IO ()
compileSrc flag = do
  makeDir objDir targetDir
  generateObjFiles flags objDir
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

srcToObj :: String -> String -> String
srcToObj src objDir
  | srcDir `isPrefixOf` src = printf "%s%s.o" objDir (takeWhile ( /= '.') obj)
  | otherwise               = undefined
  where
    Just obj = stripPrefix srcDir src

generateObj :: [String] -> String -> [String] -> IO ()
generateObj flags objDir =
  foldMap
    (\x ->
       runProcess
         cc
         (["-o"] <> [srcToObj x objDir] <> cFlags <> flags <> ["-c"] <> [x]))

generateObjFiles flags objDir = do
  objs <- obj objDir
  srcs <- src
  updatedSrcs <- updatedSrcFiles srcs objs
  generateObj flags objDir updatedSrcs

updatedSrcFiles :: [String] -> [String] -> IO [String]
updatedSrcFiles srcs objs = aux [] srcs objs
  where
    aux :: [String] -> [String] -> [String] -> IO [String]
    aux acc [] _                  = return acc
    aux acc srcs []               = return $ acc <> srcs
    aux acc (src:srcs) (obj:objs) = do
      exist <- fileExist obj
      if not exist
        then aux (src : acc) srcs objs
        else do
          rebuild <- isNewer src obj
          if rebuild
            then aux (src : acc) srcs objs
            else aux acc srcs objs

isNewer :: String -> String -> IO Bool
isNewer a b = do
  statusA <- getFileStatus a
  statusB <- getFileStatus b
  return $ modificationTime statusA > modificationTime statusB

compileCommand :: [String] -> String -> [String] -> CreateProcess
compileCommand flags targetDir = proc cc . (compileArgs flags targetDir)

compileArgs :: [String] -> String -> [String] -> [String]
compileArgs flags targetDir obj =
  obj <> flags <> cFlags <> linkingFlags <> ["-o", targetPath targetDir]

runBinary :: [Flag] -> IO ()
runBinary flags = void $ runCommand $ targetPath $ targetDir flags
  where
    targetDir []               = "target/debug"
    targetDir (FlagDebug:xs)   = "target/debug"
    targetDir (FlagRelease:xs) = "target/release"

cleanWorkingDir :: IO ()
cleanWorkingDir = runProcess "rm" ["-rf", targetDir, objDir]

compileBuild :: IO ()
compileBuild =
  runProcess "ghc" ["-O2", "-W", "-no-keep-hi-files", "-no-keep-o-files", "Build.hs"]

data SubCmd
  = SubCmdBuild
  | SubCmdRun
  | SubCmdClean
  | SubCmdCompileBuild
  | SubCmdForce
  | SubCmdForceRun

data Flag
  = FlagDebug
  | FlagRelease

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc [FlagDebug]
parseArgs args = executeSubCommand $ pairArgs subCommand flags
  where
    executeSubCommand :: Either (SubCmd, [Flag]) String -> IO ()
    executeSubCommand (Left (SubCmdBuild, flags))            = compileSrc flags
    executeSubCommand (Left (SubCmdRun, flags))              =
      compileSrc flags >> runBinary flags
    executeSubCommand (Left (SubCmdClean, flags))            = cleanWorkingDir
    executeSubCommand (Left (SubCmdCompileBuild, flags))     = compileBuild
    executeSubCommand (Left (SubCmdForce, flags))            =
      cleanWorkingDir >> compileSrc flags
    executeSubCommand (Left (SubCmdForceRun, flags))         =
      cleanWorkingDir >> compileSrc flags >> runBinary flags
    executeSubCommand (Right msg)                            = putStrLn msg >> die
    pairArgs ::
         Either SubCmd String
      -> Either [Flag] String
      -> Either (SubCmd, [Flag]) String
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
      | otherwise = Right $ printf "Invalid subcommand: %s" cmd
    matchFlag :: String -> Either Flag String
    matchFlag flag
      | flag `elem` ["--debug"]   = Left FlagDebug
      | flag `elem` ["--release"] = Left FlagRelease
      | otherwise = Right $ printf "Invalid flag: %s" flag
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
    parseSubCommand []  = Left SubCmdBuild
    parseSubCommand [x] = matchSubCommand x
    parseSubCommand _   = Right "Too many subcommands provided"
    die = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= parseArgs
