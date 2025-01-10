module Day23 where

import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Function (on)
import Data.Graph (graphFromEdges)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl', maximumBy, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybe, maybeToList)
import Data.PQueue.Prio.Max qualified as Q
import Data.Semigroup (Max (..))
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2023

toInt lenY (x, y) = x * lenY + y

readInput s = (a', nv IM.! start1, nv IM.! toInt lenY end1, startn1 + endn1)
  where
    ss = lines s
    m = drawArray @Array ss
    b@((_, minY), (_, maxY)) = A.bounds m
    lenY = maxY + 1
    start0 = head [p | (p@(_, y), '.') <- A.assocs m, y == minY]
    end0 = head [p | (p@(_, y), '.') <- A.assocs m, y == maxY]
    vn = IM.fromList $ zip [0 ..] $ IM.keys m1
    nv = IM.fromList $ flip zip [0 ..] $ IM.keys m1
    a' = A.genArray @Array (0, IM.size m1 - 1) (map (bimap (nv IM.!) Max) . (m1 IM.!) . (vn IM.!))
    (m0, end1, endn1) = go Set.empty IM.empty (end0, 0) [start0]
    (start1, startn1) = head (m0 IM.! toInt lenY start0)
    m1 = IM.delete (toInt lenY start0) m0
    go vis acc (end1, n1) [] = (IM.insert (toInt lenY end1) [] acc, end1, n1)
    go vis acc (end1, n1) (x : xs)
      | x == end0 || x `Set.member` vis = go vis acc (end1, n1) xs
      | otherwise = go vis' acc' (end1', n1') xs'
      where
        vis' = Set.insert x vis
        (acc', xs', end1', n1') = foldl' f (acc, xs, end1, n1) nexts
        nexts =
          [ i
            | d <- [minBound .. maxBound],
              let i = walk d x,
              i `Set.notMember` vis',
              A.inRange b i,
              m A.! i `elem` ['.', dirToChar d]
          ]
        f (acc, xs, end1, n1) i = go (Set.fromList [i, x]) 1 i
          where
            go vis n i
              -- \| traceShow (end1, n1) False = undefined
              | i == end0 = (acc, xs, x, n)
              | Just d' <- charToDir c', i'' <- walk d' i' = (IM.insertWith (<>) (toInt lenY x) [(toInt lenY i'', n + 2)] acc, i'' : xs, end1, n1)
              | c' == '.' = go (Set.insert i' vis) (n + 1) i'
              where
                (i', c') =
                  head
                    [ (i', c')
                      | d' <- [minBound .. maxBound],
                        let i' = walk d' i,
                        i' `Set.notMember` vis,
                        A.inRange b i',
                        let c' = m A.! i',
                        c' /= '#'
                    ]

walk d (x, y) = bimap (+ x) (+ y) (toIndex d)

solve m s0 end = go mempty [(s0, notTravelled, Max 0)]
  where
    notTravelled = IM.unionsWith (<>) $ map IM.fromList $ A.elems m
    go nMax [] = nMax
    go nMax ((s, nt, n) : xs)
      | s == end = go (nMax <> n) xs
      | nMax > n + sum nt = go nMax xs
      | otherwise = go nMax xs'
      where
        xs' = foldr g xs (m A.! s)
        g (s', n') accX
          | s' `IM.notMember` nt = accX
          | otherwise = (s', IM.delete s' nt, n'') : accX
          where
            n'' = n + n'

buildGraph m = graphFromEdges [(i, i, map fst (m IM.! i)) | i <- IM.keys m]

expand s0 m = go Set.empty m' [s0]
  where
    b = A.bounds m
    l = A.assocs m
    f acc (k, xs) = foldl' (\acc (x, y) -> (x, [(k, y)]) : acc) acc xs
    m' = A.accumArray @Array (<>) [] b (foldl' f l l)
    go tra acc [] = acc
    go tra acc (x : xs) = go tra' acc' xs'
      where
        tra' = Set.insert x tra
        (acc', xs') = foldr f (acc, xs) nexts
        f i (acc, xs) = (acc A.// [(i, filter ((/= x) . fst) (acc A.! i))], xs <> [i])
        nexts =
          [ i
            | let is = m' A.! x,
              length is <= 3,
              (i, _) <- is,
              i `Set.notMember` tra',
              let ls = m' A.! i,
              length ls <= 3
          ]

day23 :: IO ()
day23 = do
  (m, start, end, n) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- (m, start, end, n) <- readInput <$> (getDataDir >>= readFile . (++ "/input/test23.txt"))
  let m' = expand start m
  putStrLn
    . ("day23a: " ++)
    . show
    $ getMax @Int (solve m start end) + n
  putStrLn
    . ("day23b: " ++)
    . show
    $ getMax @Int (solve m' start end) + n

charToDir :: Char -> Maybe Direction
charToDir 'v' = Just South
charToDir '^' = Just North
charToDir '<' = Just West
charToDir '>' = Just East
charToDir _ = Nothing

dirToChar :: Direction -> Char
dirToChar South = 'v'
dirToChar North = '^'
dirToChar West = '<'
dirToChar East = '>'
