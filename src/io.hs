module Io(isempty, mkfname, writetofile) where

import System.IO

isempty :: FilePath -> IO Bool
isempty fp =
  do
    sz <- withFile fp ReadMode hFileSize
    return (sz == 0)


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
