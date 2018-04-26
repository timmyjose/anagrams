module Main where

import System.Environment

import Data.List
import Data.Char
import Data.Maybe
import Data.Ord (comparing)

import Safe

import Lib

{--



--}

main :: IO ()
main =
  do
    putStrLn ""
    jumble <- getLine
    contents <- readFile "./data/words-dwyl.txt"
    let ws = lines contents
    let dirname = "./data/"
    let prefix = "words"
    let extn = ".txt"
    let lenwords = sort $ nub $ map length ws
    --putStrLn ("lenwords = " ++ show lenwords)
    let wdist = fromJust $ tailMay $ distwords $ ws   --- Omit single-letter words.
    --putStrLn ("wdist = " ++ show (take 6 $ fromJust $ headMay wdist))
    let lwords = wdist !! ( (length jumble) -2 ) --- Take only words whose length is the same as that of the jumbled word.
    --putStrLn ("lwords = " ++ show (take 6 lwords))
    let agms = concat $ map (++ " ") $ getagms jumble lwords
    putStrLn ("Anagrams of " ++ jumble ++ ": " ++ agms)
    --let fnames = map (mkfname prefix extn) $ lenwords
    --putStrLn ("fnames = " ++ show fnames)
    --sequence $ map writetofile $ wdist

    main
    return ()

distwords :: [String] -> [[String]]
distwords [] = []
distwords ws = groupBy pgroup $ sortBy psort $ ws
  where
    psort = comparing length
    pgroup = \x y -> (length x) == (length y)

getagms :: String -> [String] -> [String]
getagms "" _ = []
getagms s [] = []
getagms s ws = sort $ filter (not . null) $ map (getifagmof s) ws

getifagmof :: String -> String -> String
getifagmof "" _ = ""
getifagmof s w | (s /= w) && (s' /= w') && ((sort s') == (sort w')) = w'  --- Ignore case.
               | otherwise = ""
  where
    s' = map toLower s
    w' = map toLower w

mkfname :: String -> String -> Int -> String
mkfname _ _ 0 = ""
mkfname p e n = p ++ (show n) ++ e

writetofile :: [String] -> IO ()
writetofile [] = return ()
writetofile ss = writeFile fname ss'
  where
    fname = "./data/" ++ "words" ++ num ++ ".txt"
    num = show $ length $ head ss
    ss' = concat $ map (++ "\n") ss
