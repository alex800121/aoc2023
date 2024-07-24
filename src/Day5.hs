module Day5 where

import Data.Function (on)
import Data.List (foldl', scanl', sortBy)
import Data.List.Split (chunksOf, splitOn)
import Debug.Trace (traceShow)
import Paths_AOC2023

type Map = [(Int, Int, Int)]

readMap :: String -> Map
readMap = sortBy (compare `on` (\(_, y, _) -> y)) . map ((\(x : y : z : _) -> (y, y + z, x - y)) . map read . words) . tail . lines

useMap :: Map -> Int -> Int
useMap [] i = i
useMap ((start, finish, offset) : xs) i
  | i < start = i
  | i < finish = i + offset
  | otherwise = useMap xs i

fixMap :: Map -> Map
fixMap ((s, f, o) : xs)
  | s > 0 = (0, s, 0) : (s, f, o) : xs
  | otherwise = (s, f, o) : xs

useMapRange :: Map -> (Int, Int) -> [(Int, Int)]
useMapRange [] i = [i]
useMapRange ((start, end, offset) : xs) (sStart, sEnd) = lowerRange ++ midRange ++ upperRange
  where
    lowerRange = [(x, y) | let x = sStart, let y = min start sEnd, x < y]
    midRange = [(x + offset, y + offset) | let y = min end sEnd, let x = max start sStart, x < y]
    upperRange = concatMap (useMapRange xs) [(x, y) | let x = max end sStart, let y = sEnd, x < y]

day5 :: IO ()
day5 = do
  -- x : xs <- splitOn "\n\n" <$> readFile "input/test5.txt"
  x : xs <- splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let input = map (fixMap . readMap) xs
      seeds = map (read @Int) . tail . words $ x
      seedRange = map (\(x : y : _) -> (x, x + y)) $ chunksOf 2 seeds
  putStrLn
    . ("day5a: " ++)
    . show
    . minimum
    $ map (\x -> foldl' (flip useMap) x input) seeds
  putStrLn
    . ("day5b: " ++)
    . show
    . minimum
    . map fst
    $ concatMap (\x -> foldl' (\acc y -> concatMap (useMapRange y) acc) [x] input) seedRange

-- \$ map (\x -> scanl' (\acc y -> concatMap (useMapRange y) acc) [x] input) seedRange
