module Day23 where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import MyLib
import Paths_AOC2023

type M = Map Index (Map Index Int)

type A = UArray Index Char

type Index = (Int, Int)

charToDir :: Char -> Direction
charToDir 'v' = South
charToDir '^' = North
charToDir '<' = West
charToDir '>' = East

dirToChar :: Direction -> Char
dirToChar South = 'v'
dirToChar North = '^'
dirToChar West = '<'
dirToChar East = '>'

buildM :: A -> Index -> Index -> M
buildM a end start
  | Map.null nexts || start == end = Map.empty
  | otherwise = Map.unionsWith Map.union (Map.singleton start nexts : map (buildM a end) (Map.keys nexts))
  where
    nexts = buildMFrom a end start

buildMFrom :: A -> Index -> Index -> Map Index Int
buildMFrom a end start = f (Set.singleton start) initStarts 1 Map.empty
  where
    b = bounds a
    initStarts =
      Set.fromList $
        mapMaybe
          ( \d ->
              let c = dirToChar d
                  i = toIndex d
                  x = bimap (+ fst start) (+ snd start) i
                  c' = a ! x
               in if inRange b x && (c' `elem` [c, '.'])
                    then Just x
                    else Nothing
          )
          [minBound .. maxBound]
    f visited starts i acc
      -- \| traceShow starts False = undefined
      | Set.null starts = acc
      | otherwise = f visited' starts' (i + 1) acc'
      where
        visited' = Set.union visited starts
        (ended, start') = Set.partition (== end) starts
        (interest, starts') =
          Set.partition ((/= '.') . (a !))
            . Set.filter (\x -> inRange b x && x `Set.notMember` visited' && a ! x /= '#')
            . Set.unions
            $ map (\(x, y) -> Set.map (bimap (+ x) (+ y)) start') adjacent
        interest' = Set.map (\k -> bimap (+ fst k) (+ snd k) (toIndex $ charToDir $ a ! k)) interest
        acc' = Map.union (Map.fromSet (const (i + 2)) interest') $ Map.union (Map.fromSet (const i) ended) acc

adjacent = [(0, 1), (0, -1), (-1, 0), (1, 0)]

calcDist :: M -> Index -> Index -> [Int]
calcDist m end start
  | end == start = return 0
  | otherwise = do
      (next, dist) <- Map.toList $ m Map.! start
      let m' = Map.map (Map.delete start) $ Map.delete start m
      (dist +) <$> calcDist m' end next

expandM :: M -> M
expandM =
  Map.foldlWithKey'
    ( \a k b ->
        Map.unionWith Map.union (Map.map (Map.singleton k) b) $ Map.insertWith Map.union k b a
    )
    Map.empty

day23 :: IO ()
day23 = do
  input <- drawArray @UArray . lines <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- input <- drawArray @UArray . lines <$> readFile "input/test23.txt"
  let b = bounds input
      start = (fst (fst b) + 1, snd (fst b))
      end = (fst (snd b) - 1, snd (snd b))
      m = buildM input end start
      expanded = expandM m
  putStrLn
    . ("day23a: " ++)
    . show
    . maximum
    $ calcDist m end start
  putStrLn
    . ("day23b: " ++)
    . show
    . maximum
    $ calcDist expanded end start
