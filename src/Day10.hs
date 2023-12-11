module Day10 where

import Data.Bifunctor (Bifunctor (..))
import Data.Ix (Ix (inRange))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (Direction (..), drawMap)

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

walkPipe :: Map Index [Direction] -> Index -> (Index, Direction) -> (Set Index, (Set Index, Set Index)) -> (Set Index, (Set Index, Set Index))
walkPipe m initStart (i, d) acc
  -- \| traceShow (i, d, i') False = undefined
  | i' == initStart = second (bimap (Set.\\ fst acc') (Set.\\ fst acc')) acc'
  | otherwise = walkPipe m initStart (i', d') acc'
  where
    f x = bimap (+ fst x) (+ snd x)
    turnLeft = pred d
    turnRight = succ d
    i' = f i $ toIndex d
    Just d' = find (/= pred turnLeft) $ m Map.! i'
    acc'' = bimap (Set.insert i) (bimap (Set.insert $ f i $ toIndex turnLeft) (Set.insert $ f i $ toIndex turnRight)) acc
    acc' = second (bimap (Set.insert $ f i' $ toIndex turnLeft) (Set.insert $ f i' $ toIndex turnRight)) acc''

adjacent = [north, south, east, west]

bfs'' ks = bfs' ks Set.empty

bfs' ks visited start
  | not (all (inRange (minXY, maxXY)) start') = Set.empty
  | Set.null start' = visited'
  | otherwise = bfs' ks visited' start'
  where
    xs = Set.map fst ks
    ys = Set.map snd ks
    minXY = (minimum xs, minimum ys)
    maxXY = (maximum xs, maximum ys)
    visited' = Set.union visited start
    start' = (Set.unions (map (\(x, y) -> Set.map (bimap (+ x) (+ y)) start) adjacent) Set.\\ visited') Set.\\ ks

day10 :: IO ()
day10 = do
  input' <- drawMap direction . lines <$> readFile "input/input10.txt"
  let start' = Map.findMin $ Map.filter ((== 4) . length) input'
      start = second (filter (\x -> x `elem` (maybe [] (map (succ . succ)) . (input' Map.!?) . bimap (+ fst (fst start')) (+ snd (fst start'))) (toIndex x))) start'
      startB = second head start
      input = uncurry Map.insert start input'
      (day10a, day10b) =
        (\(x, (y, z)) -> (length x `div` 2, length (bfs'' x y) + length (bfs'' x z))) $
          walkPipe input' (fst startB) startB (Set.empty, (Set.empty, Set.empty))
  putStrLn
    . ("day10a: " ++)
    . show
    $ day10a
  putStrLn
    . ("day10b: " ++)
    . show
    $ day10b
