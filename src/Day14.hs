{-# LANGUAGE LambdaCase #-}

module Day14 where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl', transpose, unfoldr)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M
import Data.Word
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawArray, drawGraph, firstCycle, firstCycle', firstRepeat, firstRepeat')
import Paths_AOC2023

readInput' s = (n, V.fromList @Int (concatMap (map (\case '.' -> 0; '#' -> 1; 'O' -> 2)) ss))
  where
    n = length ss
    ss = lines s

showRocks' n len rocks = unlines . f . chunksOf len . map (\case 0 -> '.'; 1 -> '#'; 2 -> 'O') $ V.toList rocks
  where
    counterclockwise = reverse . transpose
    f = (!! (n `mod` 4)) . iterate counterclockwise

calcRocks' n len rocks = V.sum . V.map f $ V.elemIndices 2 rocks
  where
    f = case n of
      0 -> (len -) . (`div` len)
      1 -> (1 +) . (`mod` len)
      2 -> (1 +) . (`div` len)
      3 -> (len -) . (`mod` len)

roll' len !rocks = V.create $ do
  v <- M.replicate (len * len) 0
  let f !x !y !z
        | x >= len = pure ()
        | y >= len = f (x + 1) 0 0
        | c == 0 = f x (y + 1) z
        | c == 1 = M.write v (x * len + len - y - 1) 1 >> f x (y + 1) (y + 1)
        | c == 2 = M.write v (x * len + len - z - 1) 2 >> f x (y + 1) (z + 1)
        where
          c = rocks V.! (x + len * y)
  f 0 0 0
  pure v

cycleRoll' len !rocks = f . f . f . f $ rocks
  where
    f = roll' len

day14 :: IO ()
day14 = do
  input <- getDataDir >>= readFile . (++ "/input/input14.txt")
  -- input <- getDataDir >>= readFile . (++ "/input/test14.txt")
  let (len, rocks0) = readInput' input
      xs0 = iterate (roll' len) rocks0
      ys0 = iterate (cycleRoll' len) rocks0
      Just (y, c) = detectCycle ys0
      n = (1000000000 - y) `mod` c + y
  putStrLn
    . ("day14a: " ++)
    . show
    . calcRocks' 1 len
    $ xs0 !! 1
  putStrLn
    . ("day14b: " ++)
    . show
    . calcRocks' 0 len
    $ ys0 !! n

detectCycle xs = go 0 1 xs xs
  where
    go a b (x : xs) (_ : y : ys)
      | x == y = Just (a, b - a)
      | otherwise = go (a + 1) (b + 2) xs ys
    go _ _ _ _ = Nothing

-- type Index = (Int, Int)
--
-- type Stones = Array Index Word8
--
-- type Rounded = (Int, IntMap (IntMap Int))
--
-- type Cubes = [Array (Int, Int) Int]
--
-- readInput s = (b, cubes, (3, rounded))
--   where
--     a = drawArray @Array (lines s)
--     b@((minx, miny), (maxx, maxy)) = A.bounds a
--     zerox = IM.fromList [(x, IM.singleton (-1) 0) | x <- [minx .. maxx]]
--     zeroy = IM.fromList [(y, IM.singleton (-1) 0) | y <- [miny .. maxy]]
--     cubes =
--       [ A.array @Array b l
--         | (b, l) <-
--             [ ( ((minx, miny), (maxx, maxy)),
--                 [ j
--                   | x <- [minx .. maxx],
--                     j <- f (second (+ 1), first (maxx -), (+ 1) . snd, (x, miny), 0)
--                 ]
--               ),
--               ( ((miny, minx), (maxy, maxx)),
--                 [ j
--                   | y <- [miny .. maxy],
--                     j <- f (first (+ 1), \(x, y) -> (y, x), (+ 1) . fst, (minx, y), 0)
--                 ]
--               ),
--               ( ((minx, miny), (maxx, maxy)),
--                 [ j
--                   | x <- [minx .. maxx],
--                     j <- f (second (subtract 1), second (maxy -), (+ 1) . (maxy -) . snd, (x, maxy), 0)
--                 ]
--               ),
--               ( ((miny, minx), (maxy, maxx)),
--                 [ j
--                   | y <- [miny .. maxy],
--                     j <- f (first (subtract 1), \(x, y) -> (maxy - y, maxx - x), (+ 1) . (maxx -) . fst, (maxx, y), 0)
--                 ]
--               )
--             ]
--       ]
--     rounded = IM.fromListWith (IM.unionWith (+)) [(maxy - y, IM.singleton (maxx - x) 1) | ((x, y), 'O') <- A.assocs a]
--     g acc (x, y) = IM.insertWith (<>) x (IS.singleton y) acc
--     f (next, fi, fn, i@(x, y), n) = case a A.!? i of
--       Nothing -> []
--       Just '#' -> (fi i, n') : f (next, fi, fn, i', n')
--       Just _ -> (fi i, n) : f (next, fi, fn, i', n)
--       where
--         n' = fn i
--         i' = next i
--
-- roll :: Cubes -> Rounded -> Rounded
-- roll cubes (r0, rocks0) = (r1, rocks1)
--   where
--     r1 = (r0 + 1) `mod` 4
--     cube = cubes !! r1
--     m = snd (snd (A.bounds cube))
--     rocks1 =
--       IM.fromListWith
--         (IM.unionWith (+))
--         [ (y, IM.singleton (cube A.! (y, m - x)) 1)
--           | (x, ys) <- IM.toList rocks0,
--             (y0, n) <- IM.toList ys,
--             y <- [y0 .. y0 + n - 1]
--         ]
--
-- calcRounded :: Cubes -> Rounded -> Int
-- calcRounded cubes (n, m) =
--   sum
--     [ f n (x, y)
--       | (x, ys) <- IM.toList m,
--         (y0, n0) <- IM.toList ys,
--         y <- [y0 .. y0 + n0 - 1]
--     ]
--   where
--     cube = cubes !! ((n - 1) `mod` 4)
--     (_, (maxx, maxy)) = A.bounds cube
--     f 0 (x, y) = maxy + 1 - y
--     f 1 (x, y) = maxx + 1 - x
--     f 2 (x, y) = 1 + y
--     f 3 (x, y) = 1 + x
--
-- showRounded :: Cubes -> Rounded -> String
-- showRounded cubes (n, m) =
--   unlines
--     . drawGraph (\case Just _ -> 'O'; _ -> '.')
--     $ Map.fromList
--       [ (f n (x, y), ())
--         | (x, ys) <- IM.toList m,
--           (y0, n0) <- IM.toList ys,
--           y <- [y0 .. y0 + n0 - 1]
--       ]
--   where
--     cube = cubes !! ((n - 1) `mod` 4)
--     (_, (maxx, maxy)) = A.bounds cube
--     f 0 (x, y) = (maxx - x, y)
--     f 1 (x, y) = (y, x)
--     f 2 (x, y) = (x, maxy - y)
--     f 3 (x, y) = (maxy - y, maxx - x)
--
-- cycleRoll cubes m = iterate (roll cubes) m !! 4
--
-- day14 :: IO ()
-- day14 = do
--   input <- getDataDir >>= readFile . (++ "/input/input14.txt")
--   let (b, cubes, rounded) = readInput input
--       l = iterate (roll cubes) rounded
--       ls = iterate (cycleRoll cubes) rounded
--       Just (c, y, z) = firstCycle' ls
--       n = (1000000000 - y) `mod` c + y
--   putStrLn
--     . ("day14a: " ++)
--     . show
--     $ calcRounded cubes (l !! 1)
--   putStrLn
--     . ("day14b: " ++)
--     . show
--     $ calcRounded cubes (ls !! n)
