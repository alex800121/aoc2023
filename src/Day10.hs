module Day10 where

import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix (inRange))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (Direction (..), drawGraph, drawMap)

type Index = (Int, Int)

type M = Map Index [Index]

north = (0, -1)

south = (0, 1)

east = (1, 0)

west = (-1, 0)

toIndex :: Direction -> Index
toIndex North = north
toIndex South = south
toIndex East = east
toIndex West = west

direction :: Char -> [Direction]
direction '.' = []
direction 'S' = [North, South, East, West]
direction '-' = [East, West]
direction '|' = [North, South]
direction 'L' = [North, East]
direction 'F' = [South, East]
direction 'J' = [North, West]
direction '7' = [South, West]

bfs :: M -> Set Index -> Set Index -> Int -> Int
bfs m visited start acc
  | Set.null next = acc
  | otherwise = bfs m visited' next (acc + 1)
  where
    visited' = visited `Set.union` start
    next =
      (Set.\\ visited')
        . Set.unions
        $ Set.map
          ( \x ->
              let xs = filter (\y -> x `elem` fromMaybe [] (m Map.!? y)) . fromMaybe [] $ m Map.!? x
               in Set.fromList xs
          )
          start

adjacent = [north, south, east, west]

day10 :: IO ()
day10 = do
  -- input' <- Map.mapWithKey (\(kx, ky) a -> map (bimap (+ kx) (+ ky)) a) . drawMap (Just . direction) . lines <$> readFile "input/test10.txt"
  input'' <- drawMap Just . lines <$> readFile "input/input10.txt"
  let input' = Map.mapWithKey (\(kx, ky) -> map (bimap (+ kx) (+ ky) . toIndex) . direction) input''
      start' = Map.findMin $ Map.filter ((== 4) . length) input'
      start = second (filter ((fst start' `elem`) . fromMaybe [] . (input' Map.!?))) start'
      input = uncurry Map.insert start input'
      day10a = bfs input Set.empty (Set.singleton (fst start)) 0
  putStrLn
    . ("day10a: " ++)
    . show
    $ day10a
