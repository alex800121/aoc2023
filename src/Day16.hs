module Day16 where

import Control.Monad ((<=<))
import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array, Ix (..), bounds, (!))
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Sum (..))
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
        [ (x, IS.singleton y)
          | ((x, y), c) <- A.assocs m,
            c `notElem` ".|"
        ]
    ymap =
      IM.fromListWith
        (<>)
        [ (y, IS.singleton x)
          | ((x, y), c) <- A.assocs m,
            c `notElem` ".-"
        ]

toIntD lenY ((x, y), d) = (x * lenY + y) * 4 + fromEnum d

toInt lenY (x, y) = x * lenY + y

skipWalk (xmap, ymap) m start = IS.size $ go IS.empty IS.empty [start]
  where
    b@((minx, miny), (maxx, maxy)) = A.bounds m
    lenY = maxy - miny + 1
    go !acc _ [] = acc
    go !acc visited (beam@(i@(x, y), d) : xs)
      | intx `IS.member` visited = go acc visited xs
      | otherwise = go acc' visited' xs'
      where
        intx = toIntD lenY beam
        visited' = IS.insert intx visited
        ds = maybe [] (`reflect` d) (m A.!? i)
        buildPoints' = buildPoints lenY
        (acc', xs') = foldl' f (acc, xs) ds
        f (acc, xs) d = case d of
          North -> g acc xs d (x, miny) (second . const) (IS.maxView . fst) xmap y x
          South -> g acc xs d (x, maxy) (second . const) (IS.minView . snd) xmap y x
          West -> g acc xs d (minx, y) (first . const) (IS.maxView . fst) ymap x y
          East -> g acc xs d (maxx, y) (first . const) (IS.minView . snd) ymap x y
        g acc xs d a fb mm xymap xy0 xy1 =
          maybe
            (buildPoints' [i, a] <> acc, xs)
            (\(xy, _) -> (buildPoints' [i, fb xy (x, y)] <> acc, (fb xy (x, y), d) : xs))
            (mm . IS.split xy0 =<< (xymap IM.!? xy1))

buildPoints _ [] = IS.empty
buildPoints lenY [x] = IS.singleton (toInt lenY x)
buildPoints lenY ((x0, y0) : z@(x1, y1) : zs) = IS.fromList [toInt lenY (x, y) | x <- xs, y <- ys] <> buildPoints lenY (z : zs)
  where
    xs = [min x0 x1 .. max x0 x1]
    ys = [min y0 y1 .. max y0 y1]

reflect :: Char -> Direction -> [Direction]
reflect '\\' d = if odd (fromEnum d) then [succ d] else [pred d]
reflect '/' d = if odd (fromEnum d) then [pred d] else [succ d]
reflect '|' d | odd (fromEnum d) = [pred d, succ d]
reflect '-' d | even (fromEnum d) = [succ d, pred d]
reflect _ d = [d]

getStarts m =
  [((x, y), d) | y <- [miny .. maxy], (x, d) <- [(minx, East), (maxx, West)]]
    <> [((x, y), d) | x <- [minx .. maxx], (y, d) <- [(miny, South), (maxy, North)]]
  where
    ((minx, miny), (maxx, maxy)) = A.bounds m

walk m = IS.size . go IS.empty IS.empty
  where
    b@((_, miny), (_, maxy)) = A.bounds m
    lenY = maxy - miny + 1
    go !acc _ [] = acc
    go !acc !visited (a@(i@(ix, iy), d@(dx, dy)) : xs)
      | c, intB `IS.member` visited = go acc visited xs
      | c = go acc' visited' xs'
      | otherwise = go acc' visited xs'
      where
        acc' = IS.insert (toInt lenY (fst a)) acc
        intB = (ix * lenY + iy) * 9 + (dx + 1) * 3 + (dy + 1)
        c = (m A.! i) `elem` "|-/\\"
        visited' = IS.insert intB visited
        xs' = nexts <> xs
        nexts =
          [ (i', d')
            | d' <- case m A.!? i of
                Nothing -> []
                Just c -> reflect' c d,
              let i' = bimap (+ fst i) (+ snd i) d',
              A.inRange b i'
          ]
    walked l = mconcat $ A.elems $ A.accumArray @Array (\_ _ -> Sum 1) (Sum 0) b l

reflect' '/' (dx, dy) | dy == 0 = [(dy, -dx)]
reflect' '/' (dx, dy) | dx == 0 = [(-dy, dx)]
reflect' '\\' (dx, dy) = [(dy, dx)]
reflect' '|' (dx, dy) | dy == 0 = [(0, 1), (0, -1)]
reflect' '-' (dx, dy) | dx == 0 = [(1, 0), (-1, 0)]
reflect' _ d = [d]

day16 :: IO ()
day16 = do
  input <- drawArray @Array . lines <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  -- input <- drawArray @Array . lines <$> (getDataDir >>= readFile . (++ "/input/test16.txt"))
  let (b, xymap) = buildXYMap input
  putStrLn
    . ("day16a: " ++)
    . show
    $ skipWalk xymap input ((0, 0), East)
  putStrLn
    . ("day16b: " ++)
    . show
    . maximum
    . parMap rpar (skipWalk xymap input)
    $ getStarts input
