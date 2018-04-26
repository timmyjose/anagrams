module Main where

import System.Environment

import Data.List (sort, sortBy, groupBy)

import Io (mkfname, writetofile)
import Lib (srtwrds, lwrwrds, srtprs, grpprs, fltrgrps, gatherangms, getpairs)

{--



--}

main :: IO ()
main =
  do
    putStrLn ""

    --jumble <- getLine

    let dirname = "./data/"
    let prefix = "words"
    let extn = ".txt"

    contents <- readFile "./data/words-dwyl.txt"
    --contents <- readFile "./data/words-mac.txt"

    let ws = lines contents

    let pairs = getpairs ws
    --putStrLn ("pairs = " ++ show (take 60 pairs))
    --writeFile "./data/out-pairs.txt" (show pairs)

    let grpspairs = fltrgrps $ grpprs $ srtprs pairs
    --writeFile "./data/out-grpspairs.txt" (show grpspairs)

    let agms = gatherangms grpspairs
    --putStrLn ("agms = " ++ take 600 agms)

    writeFile "./data/out.txt" agms

    --let lenwords = sort $ nub $ map length ws
    --putStrLn ("lenwords = " ++ show lenwords)
    --let wdist = fromJust $ tailMay $ distwords $ ws   --- Omit single-letter words.
    --putStrLn ("wdist = " ++ show (take 6 $ fromJust $ headMay wdist))
    --let lwords = wdist !! ( (length jumble) -2 ) --- Take only words whose length is the same as that of the jumbled word.
    --putStrLn ("lwords = " ++ show (take 6 lwords))
    --let agms = concat $ map (++ " ") $ getagms jumble lwords
    --putStrLn ("Anagrams of " ++ jumble ++ ": " ++ agms)
    --let fnames = map (mkfname prefix extn) $ lenwords
    --putStrLn ("fnames = " ++ show fnames)
    --sequence $ map writetofile $ wdist

    --main

    return ()
