module Main where

import           System.Environment (getArgs)
import           System.Directory (doesFileExist)
import           System.Process.Typed (ProcessConfig, proc, readProcess)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.List (sort, sortBy, groupBy)
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as DBC8
import qualified Data.ByteString.Lazy.Char8 as DBLC8

import Io (isempty, mkfname, writetofile)
import Lib (anagrams)
import ParserConf (getconf, getfplxcn, getfpangms, getextnfile, getfpdir)

{--


--}

main :: IO ()
main = do
  putStrLn ""
  contents <- DBC8.readFile "configuration.yaml"
  let conf = getconf contents
  --putStrLn ("conf = " ++ show conf)
  let fpa = getfpdir conf ++ getfpangms conf
  putStrLn ("fpa = " ++ fpa)
  if (  --- Checks whether the anagrams are available.
       (unsafePerformIO $ doesFileExist fpa) &&
       (not . unsafePerformIO $ isempty fpa)
    ) then do
    procinputs fpa
  else
    do
      putStrLn "Anagrams are absent! Attempting to generate."
      let fpl = getfpdir conf ++ getfplxcn conf
      putStrLn ("fpl = " ++ fpl)
      if (  --- Checks whether the lexicon is available.
           (unsafePerformIO $ doesFileExist fpl) &&
           (not . unsafePerformIO $ isempty fpl)
        ) then
        do
          contents <- readFile fpl
          writeFile fpa $ anagrams contents
          putStrLn "Anagrams generated."
          procinputs fpa
      else
        do
          putStrLn ("Lexicon is absent! Cannot generate anagrams.")
          return ()

procinputs :: String -> IO ()
procinputs fpa =
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
                prcs = proc "grep" ["-iw", jumble, fpa]
            (cdExit, stdout, stderr) <- readProcess prcs
            if show cdExit == "ExitSuccess" then
              putStrLn $ concat $ map (++ " ") $ filter (/= jumble) $ words $ DBLC8.unpack stdout
            else if show cdExit == "ExitFailure 1" then
              putStr ""
            else
              putStrLn $ show cdExit
        else
          do
            return ()
        procinputs fpa
    else
      do
        return ()
