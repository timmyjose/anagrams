module Lib (getpairs, srtwrds, lwrwrds, srtprs, grpprs, fltrgrps, gatherangms, getangms, distwords) where

import Data.List (sort, sortBy, groupBy, concat)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Safe
import Safe.Exact

getpairs :: [String] -> [(String, String)]
getpairs ws = zip (srtwrds ws) $ map sort $ lwrwrds $ srtwrds ws

srtwrds :: [String] -> [String]
srtwrds = sortBy (comparing length)

lwrwrds :: [String] -> [String]
lwrwrds = map (\x -> map toLower x)

srtprs :: [(String, String)] -> [(String, String)]
srtprs =  sortBy (comparing snd)

grpprs :: [(String, String)] -> [[(String, String)]]
grpprs =  groupBy (\x y -> (snd x == snd y))

fltrgrps :: [[(String, String)]] -> [[(String, String)]]
fltrgrps = filter ((> 1) . length)

gatherangms :: [[(String, String)]] -> String
gatherangms gs = concat $ map ( (++ "\n\n") . getangms ) gs

getangms :: [(String, String)] -> String
getangms [] = ""
getangms g = fromJust $ initMay $ concat $ map ((++ " ") . fst) g

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
