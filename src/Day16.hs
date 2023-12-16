module Day16 where

import Data.Array (Array, Ix (..), bounds, (!))
import MyLib (Direction (..), drawArray)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Set (Set)
import qualified Data.Set as Set

type Index = (Int, Int)

type Beam = (Index, Direction)

reflect :: Char -> Direction -> [Direction]
reflect '\\' d = if odd (fromEnum d) then [succ d] else [pred d]
reflect '/' d = if odd (fromEnum d) then [pred d] else [succ d]
reflect '|' d = if odd (fromEnum d) then [pred d, succ d] else [d]
reflect '-' d = if odd (fromEnum d) then [d] else [succ d, pred d]
reflect _ d = [d]

toIndex :: Direction -> Index
toIndex North = (0, -1)
toIndex South = (0, 1)
toIndex West = (-1, 0)
toIndex East = (1, 0)

day16a :: Array Index Char -> Set Beam -> Set Beam -> Set Beam
day16a a visited s
  | Set.null s' = visited
  | otherwise = day16a a visited' (Set.unions (Set.map (f a) s'))
  where
    b = bounds a
    s' = Set.filter (inRange b . fst) s Set.\\ visited
    visited' = Set.union visited s'
    f a (i, d) 
      | inRange b i = Set.fromList i'
      | otherwise = Set.empty
      where
        d' = reflect (a ! i) d
        i' = map ((,) <$> (bimap (+ fst i) (+ snd i) . toIndex) <*> id) d'

day16 :: IO ()
day16 = do
  input <- drawArray @Array . lines <$> readFile "input/input16.txt"
  -- input <- drawArray @Array . lines <$> readFile "input/test16.txt"
  print $ length $ Set.map fst $ day16a input Set.empty $ Set.singleton ((0, 0), East)
