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
import Debug.Trace (traceShow)

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

toDirection :: Index -> Direction
toDirection (0, -1) = North
toDirection (0, 1) = South
toDirection (1, 0) = East
toDirection (-1, 0) = West

direction :: Char -> Maybe [Direction]
direction 'S' = Just [North, South, East, West]
direction '-' = Just [East, West]
direction '|' = Just [North, South]
direction 'L' = Just [North, East]
direction 'F' = Just [South, East]
direction 'J' = Just [North, West]
direction '7' = Just [South, West]
direction _ = Nothing

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

walkPipe :: Map Index [Direction] -> Index -> (Index, Direction) -> (Set Index, Set Index) -> (Set Index, Set Index)
walkPipe m initStart (i, d) acc
  | traceShow (i, d, i') False = undefined
  | i' == initStart = bimap s' s' acc'
  | otherwise = walkPipe m initStart (i', d') acc'
  where
    f = bimap (+ fst i) (+ snd i)
    turnLeft = pred d
    turnRight = succ d
    i' = f $ toIndex d
    Just d' = find (/= pred turnLeft) $ m Map.! i'
    acc' = bimap (Set.insert $ f $ toIndex turnLeft) (Set.insert $ f $ toIndex turnRight) acc
    s' = (Set.\\ Map.keysSet m)

adjacent = [north, south, east, west]

day10 :: IO ()
day10 = do
  input'' <- drawMap direction . lines <$> readFile "input/test10.txt"
  -- input'' <- drawMap direction . lines <$> readFile "input/input10.txt"
  let input' = Map.mapWithKey (\(kx, ky) -> map (bimap (+ kx) (+ ky) . toIndex)) input''
      start' = Map.findMin $ Map.filter ((== 4) . length) input'
      start = second (filter ((fst start' `elem`) . fromMaybe [] . (input' Map.!?))) start'
      startB = second (head . map (toDirection . bimap (subtract $ fst (fst start)) (subtract $ snd (fst start)))) start
      input = uncurry Map.insert start input'
      day10a = bfs input Set.empty (Set.singleton (fst start)) 0
  putStrLn
    . ("day10a: " ++)
    . show
    $ day10a
  print startB
  -- print input''
  print $ walkPipe input'' (fst startB) startB (Set.empty, Set.empty)
  -- print startB
