{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.Array.Unboxed (UArray, Ix (..), bounds, (!))
import qualified Data.Array as A
import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (Direction (..), drawArray)
import Debug.Trace (traceShow)

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

calc :: UArray Index Char -> Set Beam -> Set Beam -> Set Beam
calc a visited s
  | Set.null s' = visited
  | otherwise = calc a visited' (Set.unions (Set.map (calcSingle a) s'))
  where
    b = bounds a
    s' = Set.filter (inRange b . fst) s Set.\\ visited
    visited' = Set.union visited s'

calcSingle :: UArray Index Char -> Beam -> Set Beam
calcSingle a (i, d)
  | inRange b i = Set.fromList i'
  | otherwise = Set.empty
  where
    b = bounds a
    d' = reflect (a ! i) d
    i' = map ((,) <$> (bimap (+ fst i) (+ snd i) . toIndex) <*> id) d'


day16b :: UArray Index Char -> Int
day16b a = maximum $ map (length . Set.map fst . calc a Set.empty . Set.singleton) edges
  where
    ((minX, minY), (maxX, maxY)) = bounds a
    edges =
      [((minX, y), East) | y <- [minY .. maxY]]
        ++ [((maxX, y), West) | y <- [minY .. maxY]]
        ++ [((x, minY), South) | x <- [minX .. maxX]]
        ++ [((x, maxY), North) | x <- [minX .. maxX]]

day16 :: IO ()
day16 = do
  input <- drawArray @UArray . lines <$> readFile "input/input16.txt"
  -- input <- drawArray @Array . lines <$> readFile "input/test16.txt"
  putStrLn
    . ("day16a: " ++)
    . show
    . length
    . Set.map fst
    . calc input Set.empty
    $ Set.singleton ((0, 0), East)
  putStrLn
    . ("day16b: " ++)
    . show
    $ day16b input
  -- print $ range (West, South)
