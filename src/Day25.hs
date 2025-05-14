{-# LANGUAGE TupleSections #-}

module Day25 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl', find, unfoldr)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Paths_AOC2023
import System.Random
import Control.Monad (join)
import Control.Parallel.Strategies

-- type G = Map (Set String) (MultiSet String)
type G = Map (Int, Int) (MultiSet (Int, Int))

readInput :: String -> G
readInput s =
  Map.fromList
    . map (bimap ((,1) . (allPoints Map.!)) (MS.fromList . map ((,1) . (allPoints Map.!)) . Set.toList))
    $ Map.toList s'
  where
    s' =
      Map.unionsWith (<>)
        . map f
        $ lines s
    f x = Map.fromList $ (a, Set.fromList bs) : map (,Set.singleton a) bs
      where
        [a, b] = splitOn ": " x
        bs = words b
    allPoints =
      Map.fromList
        . (`zip` [0 ..])
        . Set.toList
        $ Map.foldlWithKey' (\acc k xs -> Set.singleton k <> xs <> acc) Set.empty s'

contract :: (RandomGen seed) => seed -> G -> Maybe Int
contract seed0 !g0
  | s0 > 2 = contract seed1 gNew
  | l <= 3 = Just $ snd k0 * snd k1
  | otherwise = Nothing
  where
    s0 = Map.size g0
    (n, seed1) = random @Int seed0
    n0 = n `mod` s0
    (k0, c0) = Map.elemAt n0 g0
    k1 = MS.findMin c0
    l = MS.occur k1 c0
    c1 = g0 Map.! k1
    kNew = second (+ snd k1) k0
    cNew = MS.union (MS.deleteAll k1 c0) (MS.deleteAll k0 c1)
    gNew' = Map.insert kNew cNew $ Map.delete k1 $ Map.delete k0 g0
    gNew = foldl' f gNew' (MS.distinctElems cNew)
    f acc kc = Map.adjust (MS.insertMany kNew (MS.occur kc (gNew' Map.! kNew)) . MS.deleteAll k0 . MS.deleteAll k1) kc acc

solve !i !g = fromMaybe (solve i' g) (contract i g)
  where
    i' = snd $ genWord8 i

day25 :: IO ()
day25 = do
  m <- readInput <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  putStrLn
    . ("day25a: " ++)
    . show
    . join
    . find isJust
    . map (`contract` m)
    $ unfoldr (\x -> let (_, g) = genWord8 x in Just (g, g)) (mkStdGen 0)
