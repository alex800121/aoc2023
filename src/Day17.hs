module Day17 where

import Control.Monad (when, (>=>))
import Control.Monad.ST (ST, runST)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as Q
import Data.Vector.Mutable qualified as V
import Data.Vector.Unboxed.Mutable qualified as U
import Debug.Trace (traceShow, traceShowM)
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2023

type Index = (Int, Int)

type Q = Q.MinPQueue Int (Index, Direction)

type M = Array Index Int

astar minRun maxRun m = runST $ do
  v <- U.replicate (toInt end 1 + 1) maxBound
  bucket <- V.replicate lenBucket []
  V.write bucket (manhattan start end `mod` lenBucket) (map (0,) starts)
  mapM_ (\x -> U.write v (uncurry toInt x) 0) starts
  go v bucket (manhattan start end `mod` lenBucket)
  where
    b@(start, end) = A.bounds m
    starts = [((0, 0), d) | d <- [0, 1]]
    lenY = snd end + 1
    lenBucket = 1 + uncurry max end + 9 * maxRun
    toInt (x, y) d = (x * lenY * 2) + (y * 2) + d
    go :: U.STVector s Int -> V.STVector s [(Int, (Index, Int))] -> Int -> ST s Int
    go tra bucket ib0 = do
      xs <- V.read bucket ib
      if null xs then go tra bucket (succ ib0) else do
        V.write bucket ib []
        f [(cost, i, d `xor` 1, next) | (cost, (i, d)) <- xs, next <- [-1, 1]]
      where
        ib = ib0 `mod` lenBucket
        f [] = go tra bucket ib0
        f ((cost, i, d, nextI) : xs)
          | i == end = pure cost
          | otherwise = g 0 cost i
          where
            intI = toInt i d
            next = if d == 0 then first (+ nextI) else second (+ nextI)
            g run cost i
              | run' > maxRun || not (A.inRange b i') = f xs
              | otherwise = do
                  cost0 <- U.read tra intI'
                  when (run' >= minRun && cost' < cost0) $ U.write tra intI' cost' >> V.modify bucket ((cost', (i', d)) :) hue'
                  g run' cost' i'
              where
                i' = next i
                intI' = toInt i' d
                run' = succ run
                cost' = cost + m A.! i'
                hue' = (manhattan i' end + cost') `mod` lenBucket

manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

day17 :: IO ()
day17 = do
  input <- A.amap digitToInt . drawArray @Array . lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  -- input <- A.amap digitToInt . drawArray @Array . lines <$> (getDataDir >>= readFile . (++ "/input/test17.txt"))
  putStrLn
    . ("day17a: " ++)
    . show
    $ astar 1 3 input
  putStrLn
    . ("day17b: " ++)
    . show
    $ astar 4 10 input

-- astar minRun maxRun m end start = runST $ do
--   v <- V.replicate (toInt end maxBound + 1) maxBound
--   mapM_ (\x -> V.write v (uncurry toInt x) 0) starts
--   go v (Q.fromList (map ((manhattan start end, ) . (0, )) starts))
--   where
--     starts = [(start, d) | d <- [South, East]]
--     toInt (x, y) d = x * maxy * 2 + y * 2 + (fromEnum d `mod` 2)
--     !maxy = snd end + 1
--     go :: STVector s Int -> Q.MinPQueue Int (Int, (Index, Direction)) -> ST s (Maybe Int)
--     go travelled Q.Empty = pure Nothing
--     go travelled ((hue, g@(len, (s, d))) Q.:< qs)
--       | s == end = pure $ Just len
--       | otherwise = (f minRun maxRun 0 len (succ d) s >=> f minRun maxRun 0 len (pred d) s >=> go travelled) qs
--       where
--         f min max run n d i@(x, y) q
--           | run' > max || not (A.inRange (start, end) i') = pure q
--           | Just n' <- m A.!? i',
--             n'' <- n + n',
--             hue <- n'' + manhattan i' end = do
--               b <- V.read travelled e'
--               if run' < min || b <= n''
--                 then f min max run' n'' d i' q
--                 else
--                   V.write travelled e' n''
--                     >> f min max run' n'' d i' (Q.insert hue (n'', (i', d)) q)
--           where
--             !i' = bimap (+ x) (+ y) (toIndex d)
--             !run' = succ run
--             e' = toInt i' d
