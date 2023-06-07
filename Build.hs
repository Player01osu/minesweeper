import Script
import System.Environment
import System.Process
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

compileSrc :: IO ()
compileSrc = do
   src <- src
   makeTarget
   createProcess (proc cc (["-o", targetPath] <> src <> cflags))
   return ()

main :: IO ()
main = compileSrc
