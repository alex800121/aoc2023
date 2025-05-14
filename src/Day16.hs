{-# LANGUAGE TupleSections #-}

module Day16 where

import Control.Monad ((<=<))
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Ix (..), UArray, bounds, (!))
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawArray, drawGraph, toIndex)
import Paths_AOC2023

type Index = (Int, Int)

type Beam = (Index, Direction)

type XYMap = (IntMap (IntMap Char), IntMap (IntMap Char))

buildXYMap m = (b, (xmap, ymap))
  where
    b = A.bounds m
    xmap =
      IM.fromListWith
        (<>)
        [ (x, IM.singleton y c)
          | ((x, y), c) <- A.assocs m,
            c `notElem` ".|"
        ]
    ymap =
      IM.fromListWith
        (<>)
        [ (y, IM.singleton x c)
          | ((x, y), c) <- A.assocs m,
            c `notElem` ".-"
        ]

skipWalk :: (Index, Index) -> XYMap -> Beam -> Set Index
skipWalk b xymap x = go Set.empty Set.empty [x]
  where
    go acc tra [] = acc
    go acc tra (x : ss)
      | x `Set.member` tra || not (inRange b (fst x)) =
          go acc tra ss
      | (i, d) <- x,
        (g, []) <- skipStep b xymap x,
        y <- (g, d) =
          go (toIndices i g <> acc) (Set.insert y $ Set.insert x tra) ss
      | (i, d) <- x,
        (f, ds) <- skipStep b xymap x,
        y <- (f, d),
        ss' <-
          [ (bimap (+ fst f) (+ snd f) (toIndex d'), d')
            | d' <- ds
          ] =
          go (toIndices i f <> acc) (Set.insert y $ Set.insert x tra) (ss' <> ss)
    toIndices (x0, y0) (x1, y1) = Set.fromList [(x, y) | x <- [min x0 x1 .. max x0 x1], y <- [min y0 y1 .. max y0 y1]]

-- skipStep :: (Index, Index) -> XYMap -> Beam -> [Beam]
skipStep b@((minx, miny), (maxx, maxy)) (xmap, ymap) (i@(x, y), d) =
  maybe (g, []) (bimap h (`reflect` d) . fst) f
  where
    (g, h, f) = case d of
      North -> ((x, miny), (x,), IM.maxViewWithKey . IM.filterWithKey (\k _ -> k <= y) =<< (xmap IM.!? x))
      South -> ((x, maxy), (x,), IM.minViewWithKey . IM.filterWithKey (\k _ -> k >= y) =<< (xmap IM.!? x))
      West -> ((minx, y), (,y), IM.maxViewWithKey . IM.filterWithKey (\k _ -> k <= x) =<< (ymap IM.!? y))
      East -> ((maxx, y), (,y), IM.minViewWithKey . IM.filterWithKey (\k _ -> k >= x) =<< (ymap IM.!? y))

reflect :: Char -> Direction -> [Direction]
reflect '\\' d = if odd (fromEnum d) then [succ d] else [pred d]
reflect '/' d = if odd (fromEnum d) then [pred d] else [succ d]
reflect '|' d = if odd (fromEnum d) then [pred d, succ d] else [d]
reflect '-' d = if odd (fromEnum d) then [d] else [succ d, pred d]
reflect _ d = [d]

start m =
  [((x, y), d) | y <- [miny .. maxy], (x, d) <- [(minx, East), (maxx, West)]]
    <> [((x, y), d) | x <- [minx .. maxx], (y, d) <- [(miny, South), (maxy, North)]]
  where
    ((minx, miny), (maxx, maxy)) = A.bounds m

day16 :: IO ()
day16 = do
  input <- drawArray @UArray . lines <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  -- input <- drawArray @UArray . lines <$> (getDataDir >>= readFile . (++ "/input/test16.txt"))
  let (b, xymap) = buildXYMap input
  putStrLn
    . ("day16a: " ++)
    . show
    . length
    $ skipWalk b xymap ((0, 0), East)
  putStrLn
    . ("day16b: " ++)
    . show
    . maximum
    $ map (length . skipWalk b xymap) (start input)
