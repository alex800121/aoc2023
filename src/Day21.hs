{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day21 where

import qualified Data.Array.Unboxed as U
import Data.Bifunctor (Bifunctor (..))
import Data.Char (intToDigit)
import Data.List (find, unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawArray, drawGraph, toIndex)

type M = U.UArray Index Char

type Index = (Int, Int)

bfs :: U.UArray Index Char -> Map Index Int -> Map Index Int -> Map Index Int
bfs m start acc
  | Map.null start = acc
  | otherwise = bfs m start' acc'
  where
    b = U.bounds m
    acc' = Map.union acc start
    start' =
      Map.mapMaybeWithKey
        ( \k x ->
            if U.inRange b k && k `Map.notMember` acc' && (m U.! k) /= '#'
              then Just (x + 1)
              else Nothing
        )
        $ Map.unions
        $ map (\(x, y) -> Map.mapKeysMonotonic (bimap (+ x) (+ y)) start) adjacent

adjacent = map toIndex [minBound .. maxBound]

{-

#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####--S--#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
#####-----#####-----#####-----#####-----#####-----#####-----
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-----#####-----#####-----#####-----#####-----#####-----#####
-}
calcSide limit lenHalf lenSide m = e + o
  where
    (evenD, evenM) = (limit - lenHalf) `divMod` (2 * lenSide)
    even_m = Map.filter (if even lenHalf then odd else even) m
    e = (evenD * length even_m) + length (Map.filter (<= evenM) even_m)
    (oddD, oddM) = (limit - lenHalf - lenSide) `divMod` (2 * lenSide)
    odd_m = Map.filter (if even (lenHalf + lenSide) then odd else even) m
    o = (oddD * length odd_m) + length (Map.filter (<= oddM) odd_m)

calcCorner limit lenHalf lenSide m = e + o
  where
    (evenD, evenM) = (limit - (2 * lenHalf)) `divMod` (2 * lenSide)
    evenSide = 2 * evenD + 1
    evenIn = evenD * evenD
    even_m = Map.filter odd m
    e = (evenIn * length even_m) + (evenSide * length (Map.filter (<= evenM) even_m))
    (oddD, oddM) = (limit - (2 * lenHalf) - lenSide) `divMod` (2 * lenSide)
    oddSide = 2 * oddD + 2
    oddIn = (oddD + 1) * oddD
    odd_m = Map.filter even m
    o = (oddIn * length odd_m) + (oddSide * length (Map.filter (<= oddM) odd_m))

day21 :: IO ()
day21 = do
  input <- drawArray @U.UArray . lines <$> readFile "input/input21.txt"
  let Just start = fst <$> find ((== 'S') . snd) (U.assocs input)
      ((minX, minY), (maxX, maxY)) = U.bounds input
      cornerStarts =
        [ (x, y)
          | x <- [minX, maxX],
            y <- [minY, maxY]
        ]
      sideStarts = [(fst start, minY), (fst start, maxY), (minX, snd start), (maxX, snd start)]
      cornerBFS = map (\start -> bfs input (Map.singleton start 0) Map.empty) cornerStarts
      sideBFS = map (\start -> bfs input (Map.singleton start 0) Map.empty) sideStarts
      target = 26501365
      cornerN = map (calcCorner target (fst start + 1) (maxX - minX + 1)) cornerBFS
      sideN = map (calcSide target (fst start + 1) (maxX - minX + 1)) sideBFS
      day21a = bfs input (Map.singleton start 0) Map.empty
  putStrLn
    . ("day21a: " ++)
    . show
    . length
    . Map.filter ((&&) <$> (<= 64) <*> even)
    $ day21a
  putStrLn
    . ("day21b: " ++)
    . show
    $ sum cornerN + sum sideN + length (Map.filter odd day21a)
