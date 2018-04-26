module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Process (createProcess, readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort, sortBy, groupBy)

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
    putStrLn "\nEnter a word for which you want anagrams: "
    [option] <- getArgs

    if option == "-i" then
      do
        jumble <- getLine

        if (not . null) jumble then
          do
            as <- readProcess "grep" ["-iw", jumble, fa] ""
            putStrLn ("\nAnagrams of " ++ jumble ++ " are: " ++ as)
            procinputs fa
        else
          do
            return ()
    else
      do
        return ()


---     let dirname = "./data/"
---     let prefix = "words"
---     let extn = ".txt"
---
