#!/usr/bin/env stack
-- stack script --resolver ghc-9.6.7 --package split --package containers

import Data.Functor ((<&>))
import Data.List (sortOn, foldl')
import Data.List.Split (splitOn)
import Data.Set qualified as Set
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  (ranges, ids) <- parseFile
  let ranges' = mergeRanges ranges
  putStrLn $ "Task 1: " ++ show (countFreshIds ranges' ids)
  putStrLn $ "Task 2: " ++ show (allFresh ranges')

countFreshIds :: [(Integer, Integer)] -> [Integer] -> Int
countFreshIds ranges ids =
  length . Set.fromList $ [id | id <- ids, range <- ranges, inRange range id]
  where
    inRange (a, b) x = a <= x && x <= b

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges rs = reverse $ foldl' merge [] (sortOn fst rs)
  where
    merge [] r = [r]
    merge ((lo', hi') : rest) (lo, hi) | lo <= hi' + 1 = (lo', max hi hi') : rest 
                                       | otherwise     = (lo, hi) : (lo', hi') : rest

allFresh :: [(Integer, Integer)] -> Integer
allFresh = sum . map (\(lo, hi) -> hi - lo + 1) . mergeRanges

parseRange :: String -> (Integer, Integer)
parseRange s = (read a, read b)
  where
    [a, b] = splitOn "-" s

parseFile :: IO ([(Integer, Integer)], [Integer])
parseFile = do
  (getArgs >>= readFile . head)
    >>= intify
      . map lines
      . splitOn "\n\n"
  where
    intify [a, b] = return (map parseRange a, map read b)
