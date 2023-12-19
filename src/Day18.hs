{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
module Day18 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.Ix (inRange)
import Data.List (foldl', scanl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import MyLib (Direction (..), Parser, drawGraph, signedInteger, toIndex)
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

shoelace :: [Index] -> Int
shoelace ((x0, y0) : (x1, y1) : xs) = (x0 * y1) - (x1 * y0) + shoelace ((x1, y1) : xs)
shoelace _ = 0

angles :: Floating a => [Colour8] -> [((a, a) -> (a, a), (a, a) -> (a, a))]
angles ((d, i) : xs) = f (a, b) xs
  where
    g x i = if odd (fromEnum x) then undefined else undefined
      where
        (ia, ib) = bimap ((+ fst i) . (/ 2) . fromIntegral) ((+ snd i) . (/ 2) . fromIntegral) $ toIndex x
    (a, b) = undefined
    f = undefined
day18 :: IO ()
day18 = do
  input <- map (fromJust . parseMaybe readInput) . lines <$> readFile "input/test18.txt"
  -- input <- map (fromJust . parseMaybe readInput) . lines <$> readFile "input/input18.txt"
  let (hole, holeR, holeL) = dig $ map fst input
  putStrLn
    . ("day18a: " ++)
    . show
    . length
    $ countInterior hole holeR holeL
  putStrLn
    . unlines
    . drawGraph (\case Nothing -> ' '; Just _ -> '#')
    . Map.fromSet (const ())
    $ hole
