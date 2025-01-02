{-# LANGUAGE BangPatterns #-}

module Day23 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (maybe)
import Data.Semigroup (Max (..))
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2023

readInput s = (m', start, end)
  where
    ss = lines s
    m = drawArray @UArray ss
    start = head [p | (p@(_, y), '.') <- A.assocs m, y == 0]
    end = head [p | (p@(_, y), '.') <- A.assocs m, y == length ss - 1]
    m' = go Map.empty (Set.singleton start) start [start] []
    go acc _ _ [] [] = acc
    go acc travelled _ (s : ss) []
      | s `Map.member` acc = go acc travelled s ss []
      | otherwise = go acc travelled s ss (map ((1,,) <$> id <*> (`walk` s)) [minBound .. maxBound])
    go acc travelled s ss ((n, d, e) : es)
      -- \| traceShow (ss) False = undefined
      | e == end = go (Map.insertWith (<>) s [(e, n)] acc) travelled s ss es
      | n == 1, Just (dirToChar d) == m A.!? e = go acc (Set.insert e travelled) s ss ((n + 1, d, e') : es)
      | Just c <- m A.!? e,
        Just cd <- charToDir c,
        cd == d =
          go (Map.insertWith (<>) s [(e', n + 1)] acc) (Set.insert e' $ Set.insert e travelled) s (e' : ss) es
      | Just c <- m A.!? e,
        Just cd <- charToDir c,
        cd /= d =
          go acc travelled s ss es
      | e `Set.member` travelled = go acc travelled s ss es
      | maybe False (/= '#') (m A.!? e) =
          go acc (Set.insert e travelled) s ss (map ((n + 1,,) <$> id <*> (`walk` e)) [minBound .. maxBound] <> es)
      | otherwise = go acc travelled s ss es
      where
        e' = walk d e

walk d (x, y) = bimap (+ x) (+ y) (toIndex d)

-- solve m start end = go 0 notTravelled start
--   where
--     notTravelled = Map.foldl' f (Set.singleton start) m
--     f acc l = Set.fromList (map fst l) <> acc
--     go acc _ x | x == end = pure acc
--     go acc t x
--       | x `Set.notMember` t = mempty
--       | otherwise = foldMap (\(y, n) -> go (acc + n) t' y) (m Map.! x)
--       where
--         t' = Set.delete x t

solve m start end = go mempty [(start, notTravelled, Max 0)]
  where
    notTravelled = Map.foldl' f Map.empty m
    f acc l = Map.unionWith max acc (Map.fromList l)
    go nMax [] = nMax
    go nMax ((s, nt, n) : xs)
      | s == end = go (nMax <> n)  xs
      | nMax > n + sum nt = go nMax xs
      | otherwise = go nMax (xs' <> xs)
      where
        xs' =
          [ (s', Map.delete s' nt, n + n')
            | (s', n') <- m Map.! s,
              s' `Map.member` nt
          ]

expand m = Map.foldlWithKey' f m m
  where
    f acc k = foldl' (\acc (x, y) -> Map.insertWith (<>) x [(k, y)] acc) acc

day23 :: IO ()
day23 = do
  (m, start, end) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- (m, start, end) <- readInput <$> (getDataDir >>= readFile . (++ "/input/test23.txt"))
  putStrLn
    . ("day23a: " ++)
    . show
    $ getMax @Int (solve m start end)
  putStrLn
    . ("day23b: " ++)
    . show
    $ getMax @Int (solve (expand m) start end)

charToDir :: Char -> Maybe Direction
charToDir 'v' = Just South
charToDir '^' = Just North
charToDir '<' = Just West
charToDir '>' = Just East
charToDir _ = Nothing

dirToChar :: Direction -> Char
dirToChar South = 'v'
dirToChar North = '^'
dirToChar West = '<'
dirToChar East = '>'
