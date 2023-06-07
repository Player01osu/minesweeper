import Script
import System.Environment
import System.Process
import System.Exit
import Text.Printf

targetDir :: String
targetDir = "target"

srcDir :: String
srcDir = "src"

src :: IO [String]
src = outProcessLines "find" [srcDir, "-type", "f"]

makeTarget :: IO ()
makeTarget = sh $ printf "mkdir -p %s" targetDir

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

compileSrc :: IO ()
compileSrc = do
   makeTarget
   src >>= \x -> ((createProcess $ compileCommand x)
       >>= \(_, _, _, x) -> waitForProcess x)
   return ()

compileCommand :: [String] -> CreateProcess
compileCommand = proc cc . compileArgs

compileArgs :: [String] -> [String]
compileArgs src = ["-o", targetPath] <> src <> cflags <> linkingFlags

runBinary :: IO ()
runBinary = runCommand targetPath >>= (\_ -> return ())

parseArgs :: [String] -> IO ()
parseArgs [] = compileSrc
parseArgs ["build"] = compileSrc
parseArgs ["run"] = compileSrc >>= (\_ -> runBinary)

main :: IO ()
main = getArgs >>= parseArgs
