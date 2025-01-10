{-# LANGUAGE MultiWayIf #-}

module Day22 where

import Data.Array.IArray qualified as A
import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.Graph
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (drawForest)
import Debug.Trace (traceShow)
import MyLib (Parser, signedInteger)
import Paths_AOC2023
import Text.Megaparsec (parseMaybe, parseTest, sepBy)
import Text.Megaparsec.Char

type FloatBrick = [(Int, (Int, (Int, Set Index)))] -- (Key, (Z, (H, XY)))

type SettleBrick = Map Index (Int, Int) -- Map (x, y) (Z, Key)

type Support = IntMap [Int]

type Index = (Int, Int)

brickParser :: Parser (Int, (Int, Set Index))
brickParser = do
  xs <- sepBy signedInteger (char ',')
  char '~'
  ys <- sepBy signedInteger (char ',')
  let [(x0, x1), (y0, y1), (z0, z1)] = zipWith (\a b -> (min a b, max a b)) xs ys
  return (z0, (z1 - z0 + 1, Set.fromList [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]]))

floatToSettle :: FloatBrick -> (SettleBrick, Support)
floatToSettle = go Map.empty IM.empty
  where
    go :: SettleBrick -> Support -> FloatBrick -> (SettleBrick, Support)
    go settle support [] = (settle, support)
    go settle support ((k, (_, (h, xy))) : floating') = go settle' support' floating'
      where
        settle' = Map.union (Map.fromSet (const (z' + h, k)) xy) settle
        support' = IM.insert k [] $ foldl' (\acc e -> IM.insertWith (<>) e [k] acc) support s'
        (z', s') = Set.foldl' g (0, []) xy
          where
            g (z, s) xy
              | Just (z', k') <- settle Map.!? xy =
                  if
                    | z' > z -> (z', [k'])
                    | z' == z -> (z, k' : s)
                    | z' < z -> (z, s)
              | otherwise = (z, s)

calcFall support = map (\x -> Set.size (go Set.empty (Set.singleton x)) - 1) [l .. h]
  where
    depend = transposeG support
    (l, h) = A.bounds support
    go acc xs
      | Set.null xs = acc
      | otherwise = go acc' xs'
      where
        ds x = Set.fromList $ filter (\s -> all (`Set.member` acc') (depend A.! s)) $ support A.! x
        acc' = Set.union xs acc
        xs' = Set.unions $ Set.map ds xs

day22 :: IO ()
day22 = do
  (g, vToN, nToV) <-
    graphFromEdges
      . map (\(x, y) -> (x, x, y))
      . IM.toList
      . snd
      . floatToSettle
      . zip [0 ..]
      . sortBy (compare `on` fst)
      . mapMaybe (parseMaybe brickParser)
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  -- <$> (getDataDir >>= readFile . (++ "/input/test22.txt"))
  let ans = calcFall g
  putStrLn
    . ("day22a: " ++)
    . show
    . length
    $ filter (== 0) ans
  putStrLn
    . ("day22b: " ++)
    . show
    $ sum ans
