module Day8 where

import Paths_AOC2023
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (traceShow)
import MyLib (Parser, (!?))

type M = Map String (String, String)

readIns :: M -> Char -> String -> String
readIns m c = (case c of 'R' -> snd; 'L' -> fst) . (m Map.!)

day8a :: M -> [Char] -> String -> String -> Int -> Int
day8a m (x : xs) start end acc
  | start == end = acc
  | otherwise = day8a m xs start' end (acc + 1)
  where
    start' = readIns m x start

buildCycle :: M -> Vector Char -> Int -> Int -> Int -> Map String Int -> String -> Map String [Int]
buildCycle m v len pos c acc start
  | start `Map.member` acc = Map.singleton start [c]
  | last start `elem` "AZ" = Map.insertWith (++) start [c] $ buildCycle m v len (pos + 1 `mod` len) (c + 1) acc' start'
  | otherwise = buildCycle m v len (pos + 1 `mod` len) (c + 1) acc start' 
  where
    start' = readIns m (v V.! (pos `mod` len)) start
    acc' = Map.insert start pos acc

mapParser :: String -> M
mapParser s = Map.singleton start (f1, f2)
  where
    (start, s') = splitAt 3 s
    (f1, s'') = splitAt 3 $ drop 4 s'
    f2 = take 3 $ drop 2 s''

day8 :: IO ()
day8 = do
  -- ins : m : _ <- splitOn "\n\n" <$> readFile "input/test8.txt"
  ins : m : _ <- splitOn "\n\n" <$>(getDataDir >>= readFile . (++ "/input/input8.txt")) 
  let initMap = Map.unions $ map mapParser $ lines m
      a = day8a initMap (cycle ins) "AAA" "ZZZ" 0
      starts = filter ((== 'A') . last) $ Map.keys initMap
      cycles = map (buildCycle initMap (V.fromList ins) (length ins) 0 0 Map.empty) starts
      zs = map (foldr1 gcd . concat . Map.elems . Map.filterWithKey (\k _ -> last k == 'Z')) cycles
  putStrLn
    . ("day8a: " ++)
    . show
    $ a
  putStrLn
    . ("day8b: " ++)
    . show
    $ foldr1 lcm zs
