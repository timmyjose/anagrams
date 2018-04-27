module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Process.Typed (ProcessConfig, proc, readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort, sortBy, groupBy)
import qualified Data.ByteString.Lazy.Char8 as BS8

import Io (isempty, mkfname, writetofile)
import Lib (anagrams)

{--


--}

main :: IO ()
main =
  do
    let fw = "./data/words-dwyl.txt"
    let fa = "./data/anagrams.txt"
    if ( (unsafePerformIO $ doesFileExist fa) && (not . unsafePerformIO $ isempty fa) ) then
      do
        return ()
    else
      do
        contents <- readFile fw
        writeFile fa $ anagrams contents
        return ()

    procinputs fa

procinputs :: String -> IO ()
procinputs fa =
  do
    putStrLn "\nType in a word:"
    [option] <- getArgs
    --putStrLn ("\noption = " ++ option)

    if option == "-i" then
      do
        jumble <- getLine

        if (not . null) jumble then
          do
            let prcs :: ProcessConfig () () ()
                prcs = proc "grep" ["-iw", jumble, fa]
            (cdExit, stdout, stderr) <- readProcess prcs
            if show cdExit == "ExitSuccess" then
              putStrLn $ concat $ map (++ " ") $ filter (/= jumble) $ words $ BS8.unpack stdout
            else if show cdExit == "ExitFailure 1" then
              putStr ""
            else
              putStrLn $ show cdExit
        else
          do
            return ()

        procinputs fa
    else
      do
        return ()


---     let dirname = "./data/"
---     let prefix = "words"
---     let extn = ".txt"
---
