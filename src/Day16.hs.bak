module Day16 where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import MyLib (Direction (..), drawArray)
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

choices m (i, d) = maybe [] (`reflect` d) (m A.!? i)
skipWalk (xmap, ymap) m start = IS.size $ go IS.empty IS.empty [start]
  where
    b@((minx, miny), (maxx, maxy)) = A.bounds m
    lenY = maxy - miny + 1
    go _ !acc [] = acc
    go visited !acc (beam@(i@(x, y), d) : xs)
      | intx `IS.member` visited = go visited' acc xs
      | otherwise = go visited' acc' xs'
      where
        intx = toIntD lenY beam
        visited' = IS.insert intx visited
        (acc', xs') = foldl' f (acc, xs) (choices m beam)
        f (acc, xs) d = case d of
          North -> g acc xs d (x, miny) (second . const) (IS.maxView . fst) xmap y x
          South -> g acc xs d (x, maxy) (second . const) (IS.minView . snd) xmap y x
          West -> g acc xs d (minx, y) (first . const) (IS.maxView . fst) ymap x y
          East -> g acc xs d (maxx, y) (first . const) (IS.minView . snd) ymap x y
        g acc xs d a fb mm xymap xy0 xy1 =
          maybe
            (buildPoints lenY (i, a) <> acc, xs)
            (\(xy, _) -> (buildPoints lenY (i, fb xy (x, y)) <> acc, (fb xy (x, y), d) : xs))
            (mm . IS.split xy0 =<< (xymap IM.!? xy1))

buildPoints lenY ((x0, y0), (x1, y1)) = IS.fromList [toInt lenY (x, y) | x <- xs, y <- ys]
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
