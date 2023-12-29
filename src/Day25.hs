{-# LANGUAGE TupleSections #-}

module Day25 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

type G = Map (Set String) (MultiSet String)

readInput :: String -> G
readInput s = s'
  where
    s' =
      Map.unionsWith (<>)
        . map f
        $ lines s
    f x = Map.fromList $ map ((,MS.singleton a) . Set.singleton) bs ++ [(Set.singleton a, MS.fromList bs)]
      where
        [a, b] = splitOn ": " x
        bs = words b

contraction :: Int -> G -> G
contraction a g
  | 3 >= MS.size (head (Map.elems f')) = f'
  | otherwise = contraction (a + 1) g
  where
    f' = f g seed
    s = Map.size g
    seed = mkStdGen a
    f g seed
      | Map.size g <= 2 = g
      | otherwise = f g1'' seed'
      where
        s = Map.size g
        (n, seed') = first (`mod` s) $ random @Int seed
        ((g0k, g0a), g1) = (Map.elemAt n g, Map.deleteAt n g)
        kList = MS.distinctElems g0a
        k1 = kList !! (n `mod` length kList)
        ((g0k', g0a'), g1') = first Map.findMin $ Map.partitionWithKey (\k _ -> k1 `Set.member` k) g1
        g0k'' = g0k <> g0k'
        g1'' = Map.insert g0k'' (MS.filter (`Set.notMember` g0k'') $ MS.union g0a g0a') g1'

day25 :: IO ()
day25 = do
  m <- readInput <$> readFile "input/input25.txt"
  -- m <- readInput <$> readFile "input/test25.txt"
  putStrLn
    . ("day25a: " ++)
    . show
    . product
    . map length
    . Map.keys
    $ contraction 0 m
