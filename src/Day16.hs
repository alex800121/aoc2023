module Day16 where

import Control.Monad (unless)
import Control.Monad.ST.Strict (ST, runST)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.MArray qualified as M
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Ix
import Data.Monoid (Sum)
import Data.Word
import Debug.Trace (traceM)
import MyLib (Direction (..), drawArray)
import Paths_AOC2023

type Index = (Int, Int)

type Beam = (Direction, Index)

type XYMap = UArray (Direction, Index) Int

type Visited s = STUArray s Index Word8
type Output s = STUArray s Index Bool
type Input = UArray Index Char

-- 0 -> end
-- 1 -> succ -> NS=/ WE=\
-- 2 -> pred -> NS=\ WE=/
-- 3 -> - or |

-- >>> 2 ^ 8
-- 256

fromInt x = ((x `shiftR` 10, (x `shiftR` 2) .&. (2 ^ 8 - 1)), x .&. 3)

toInt ((x, y), c) = (x `shiftL` 10) + (y `shiftL` 2) + c

buildXYMap :: Input -> XYMap
buildXYMap input = A.array (bimap (minBound,) (maxBound,) b) l
  where
    b@((minx, miny), (maxx, maxy)) = A.bounds input
    l =
      [ o
        | (d, i, limit, j, sp, x) <- ds,
          o <- f d i limit j sp x
      ]
    ds =
      [(North, miny, maxy, (j,), succ, ((j, miny), 0)) | j <- [minx .. maxx]]
        <> [(South, maxy, miny, (j,), pred, ((j, maxy), 0)) | j <- [minx .. maxx]]
        <> [(East, maxx, minx, (,j), pred, ((maxx, j), 0)) | j <- [miny .. maxy]]
        <> [(West, minx, maxx, (,j), succ, ((minx, j), 0)) | j <- [miny .. maxy]]
    f :: Direction -> Int -> Int -> (Int -> Index) -> (Int -> Int) -> (Index, Int) -> [((Direction, Index), Int)]
    f d i limit j sp x
      | i == limit = [((d, i'), toInt x)]
      | otherwise = ((d, i'), toInt x) : f d (sp i) limit j sp x'
      where
        i' = j i
        x'
          | d == North || d == South = case input A.! i' of
              '\\' -> (i', 2)
              '/' -> (i', 1)
              '-' -> (i', 3)
              _ -> x
          | otherwise = case input A.! i' of
              '\\' -> (i', 1)
              '/' -> (i', 2)
              '|' -> (i', 3)
              _ -> x

reflect :: XYMap -> Beam -> (Index, [Direction])
reflect xymap b@(d, x) = case fromInt (xymap A.! b) of
  (i, 0) -> (i, [])
  (i, 1) -> (i, [succ d])
  (i, 2) -> (i, [pred d])
  (i, 3) -> (i, [succ d, pred d])

getStarts :: Input -> [Beam]
getStarts a =
  [(d, (j, i)) | (d, i) <- [(South, miny), (North, maxy)], j <- [minx .. maxx]]
    <> [(d, (i, j)) | (d, i) <- [(East, minx), (West, maxx)], j <- [miny .. maxy]]
  where
    ((minx, miny), (maxx, maxy)) = A.bounds a

calcBeam :: Input -> XYMap -> Beam -> Int
calcBeam input xymap b = runST $ do
  visited <- M.newArray b' 0
  output <- M.newArray b' False
  mapM_ (go output visited) $ cReflect input b
  length . filter id <$> M.getElems output
  where
    b' = bimap snd snd (A.bounds xymap)
    go :: Output s -> Visited s -> Beam -> ST s ()
    go output visited b@(d, i) = do
      v <- M.readArray visited i
      unless (v `testBit` di) $ do
        M.modifyArray visited i (`setBit` di)
        mapM_ (\p -> M.writeArray output p True) path
        mapM_ (\d' -> go output visited (d', j)) ds
      where
        di = fromEnum d
        (j, ds) = reflect xymap b
        path = A.range (min i j, max i j)

cReflect :: UArray Index Char -> Beam -> [Beam]
cReflect a (d, i) = [(d', i) | d' <- ds]
  where
    ds = case a A.! i of
      '\\' | d == South || d == North -> [pred d]
      '\\' -> [succ d]
      '/' | d == South || d == North -> [succ d]
      '/' -> [pred d]
      '-' | d == South || d == North -> [succ d, pred d]
      '|' | d == East || d == West -> [succ d, pred d]
      _ -> [d]

day16 :: IO ()
day16 = do
  input <- drawArray @UArray . lines <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let xymap = buildXYMap input
  putStrLn
    . ("day16a: " ++)
    . show
    $ calcBeam input xymap (East, (0, 0))
  putStrLn
    . ("day16b: " ++)
    . show
    . maximum
    . parMap rpar (calcBeam input xymap)
    $ getStarts input
