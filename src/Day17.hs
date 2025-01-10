{-# LANGUAGE MultiWayIf #-}

module Day17 where

import Control.Monad.ST (ST, runST)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
-- import Data.Array.MArray qualified as M
-- import Data.Array.ST
-- import Data.IntSet (IntSet)
-- import Data.IntSet qualified as IS
-- import Data.IntMap (IntMap)
-- import Data.IntMap qualified as IM
-- import Data.Set (Set)
-- import Data.Set qualified as Set
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.PQueue.Prio.Min qualified as Q
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as V
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2023
import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))

type Index = (Int, Int)

type Q = Q.MinPQueue Int (Index, Direction)

type M = Array Index Int

-- dijkstra minRun maxRun m end start = go (IM.fromList (map ((,0) . uncurry toInt) starts)) (Q.fromList (map (0,) starts) )
--   where
--     toInt (x, y) d = x * maxy * 2 + y * 2 + (fromEnum d `mod` 2)
--     !maxy = snd end + 1
--     starts = [(start, d) | d <- [South, East]]
--     go travelled Q.Empty = Nothing
--     go travelled ((len, g@(s, d)) Q.:< qs)
--       | s == end = Just len
--       | otherwise = go travelled' qs'
--       where
--         g = toInt s d
--         (travelled', qs') = f minRun maxRun 0 len (pred d) s $ f minRun maxRun 0 len (succ d) s (travelled, qs)
--         f min max run n d i@(x, y) (t, q)
--           | run' > max || not (A.inRange (start, end) i') = (t, q)
--           | Just n' <- m A.!? i',
--             n'' <- n + n',
--             b <- fromMaybe maxBound (t IM.!? e) =
--               if run' < min || b <= n'' then f min max run' n'' d i' (t, q) else 
--                 f min max run' n'' d i' (IM.insert e n'' t, Q.insert n'' (i', d) q)
--           where
--             !i' = bimap (+ x) (+ y) (toIndex d)
--             !run' = succ run
--             e = toInt i' d

dijkstra minRun maxRun m end start = runST $ do
  v <- V.replicate (toInt end maxBound + 1) maxBound
  mapM_ (\x -> V.write v (uncurry toInt x) 0) starts
  go v (Q.fromList (map (0,) starts))
  where
    starts = [(start, d) | d <- [South, East]]
    toInt (x, y) d = x * maxy * 2 + y * 2 + (fromEnum d `mod` 2)
    !maxy = snd end + 1
    go :: STVector s Int -> Q.MinPQueue Int (Index, Direction) -> ST s (Maybe Int)
    go travelled Q.Empty = pure Nothing
    go travelled ((len, g@(s, d)) Q.:< qs)
      | s == end = pure $ Just len
      | otherwise = (f minRun maxRun 0 len (succ d) s >=> f minRun maxRun 0 len (pred d) s >=> go travelled) qs
      where
        f min max run n d i@(x, y) q
          | run' > max || not (A.inRange (start, end) i') = pure q
          | Just n' <- m A.!? i',
            n'' <- n + n' = do
              b <- V.read travelled e'
              if run' < min || b <= n''
                then f min max run' n'' d i' q
                else
                  V.write travelled e' n''
                    >> f min max run' n'' d i' (Q.insert n'' (i', d) q)
          where
            !i' = bimap (+ x) (+ y) (toIndex d)
            !run' = succ run
            e' = toInt i' d

manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

day17 :: IO ()
day17 = do
  input <- A.amap digitToInt . drawArray @Array . lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let (start, end) = A.bounds input
  putStrLn
    . ("day17a: " ++)
    . show
    $ dijkstra 1 3 input end start
  putStrLn
    . ("day17b: " ++)
    . show
    $ dijkstra 4 10 input end start
