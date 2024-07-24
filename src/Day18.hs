{-# LANGUAGE LambdaCase #-}

module Day18 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (foldl', scanl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import MyLib (Direction (..), Parser, drawGraph, signedInteger, toIndex)
import Paths_AOC2023
import Text.Megaparsec
import Text.Megaparsec.Char

type DigPlan = (Colour8, Colour8)

type Colour8 = (Direction, Int)

type Index = (Int, Int)

type Hole = Set Index

readColour8 :: Parser Colour8
readColour8 = do
  optional $ char '#'
  let f = fromIntegral . foldl' (\acc x -> digitToInt x + 16 * acc) 0
  r <- f <$> count 5 hexDigitChar
  d <- (pred . toEnum . digitToInt) <$> anySingle
  return (d, r)

readInput :: Parser DigPlan
readInput = do
  d <- (char 'R' >> pure East) <|> (char 'D' >> pure South) <|> (char 'L' >> pure West) <|> (char 'U' >> pure North)
  space
  i <- signedInteger
  space
  c <- between (char '(') (char ')') readColour8
  pure ((d, i), c)

dig :: [Colour8] -> (Hole, Hole, Hole)
dig = go Set.empty Set.empty Set.empty (0, 0)
  where
    go acc accR accL _ [] = (acc, accR, accL)
    go acc accR accL (x, y) ((d, i) : xs)
      | i <= 0 = go acc accR accL (x, y) xs
      | otherwise = go (Set.insert i' acc) (Set.insert iR accR) (Set.insert iL accL) i' ((d, i - 1) : xs)
      where
        i' = bimap (x +) (y +) (toIndex d)
        iR = bimap (fst i' +) (snd i' +) (toIndex (succ d))
        iL = bimap (fst i' +) (snd i' +) (toIndex (pred d))

countInterior :: Hole -> Hole -> Hole -> Set Index
countInterior border r l = Set.union k $ Set.union (f Set.empty rk) (f Set.empty lk)
  where
    f visited start
      -- \| traceShow (length start) False = undefined
      | not (all (inRange b) start) = Set.empty
      | Set.null start = visited
      | otherwise = f visited' start'
      where
        start' = (Set.\\ visited) $ (Set.\\ k) $ Set.unions $ map (\(x, y) -> Set.map (bimap (+ x) (+ y)) start) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        visited' = Set.union visited start
    rk = r Set.\\ k
    lk = l Set.\\ k
    k = border
    xs = Set.map fst k
    ys = Set.map snd k
    b = ((minimum xs, minimum ys), (maximum xs, maximum ys))

shoelace :: (Num a) => [(a, a)] -> a
shoelace x = f (x ++ take 1 x)
  where
    f ((x0, y0) : (x1, y1) : xs) = (x0 * y1) - (x1 * y0) + f ((x1, y1) : xs)
    f _ = 0

angles :: (Floating a) => [Colour8] -> [((a, a) -> (a, a), (a, a) -> (a, a), (a, a) -> (a, a))]
angles = map (\(d, j) -> (a d j, g d succ j, g d pred j))
  where
    a d j = let (x, y) = toIndex d in bimap (+ fromIntegral (x * j)) (+ fromIntegral (y * j))
    g :: (Floating a) => Direction -> (Direction -> Direction) -> Int -> (a, a) -> (a, a)
    g x y j i =
      if odd (fromEnum x)
        then second (const ib) i
        else first (const ia) i
      where
        (ia, ib) = bimap ((+ fst i) . (/ 2) . fromIntegral) ((+ snd i) . (/ 2) . fromIntegral) $ toIndex $ y x

walk :: (Floating a) => [Colour8] -> [((a, a), (a, a))]
walk = f (0.5, 0.5) . angles
  where
    f i ((f0, f1, f2) : g@(g0, g1, g2) : xs) = (g1 (f1 i'), g2 (f2 i')) : f i' (g : xs)
      where
        i' = f0 i
    f _ _ = []

day18 :: IO ()
day18 = do
  -- input <- map (fromJust . parseMaybe readInput) . lines <$> readFile "input/test18.txt"
  input <- map (fromJust . parseMaybe readInput) . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let (hole, holeR, holeL) = dig $ map fst input
      frl = angles $ map snd input
      f = round . (/ 2) . abs . shoelace
  putStrLn
    . ("day18a: " ++)
    . show
    . uncurry max
    . bimap f f
    . unzip
    . walk
    $ map fst (input ++ take 1 input)
  putStrLn
    . ("day18b: " ++)
    . show
    . uncurry max
    . bimap f f
    . unzip
    . walk
    $ map snd (input ++ take 1 input)
