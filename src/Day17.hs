module Day17 where

import Data.Array.Unboxed qualified as U
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib
import Paths_AOC2023

data GameState = G
  { _location :: Index,
    _direction :: Direction,
    _moves :: Int
  }
  deriving (Show, Ord, Eq)

type Index = (Int, Int)

type Q = Q.MinPQueue Int GameState

type M = U.UArray Index Int

dijkstra :: Int -> Int -> M -> Index -> Set GameState -> Q -> Maybe Int
dijkstra minRun maxRun m end visited q = case q of
  Q.Empty -> Nothing
  (k, g) Q.:< _ | _location g == end && _moves g >= minRun && _moves g <= maxRun -> Just k
  (_, g) Q.:< xs | g `Set.member` visited -> dijkstra minRun maxRun m end visited xs
  (k, g@(G location direction moves)) Q.:< xs ->
    let visited' = Set.insert g visited
        added =
          Q.fromList $
            [ (k', G l' d' m')
              | moves >= minRun,
                let m' = 1,
                d' <- map ($ direction) [succ, pred],
                let l' = bimap (+ fst location) (+ snd location) $ toIndex d',
                U.inRange b l',
                let k' = k + m U.! l'
            ]
              ++ [ (k', G l' direction m')
                   | let m' = moves + 1,
                     m' <= maxRun,
                     let l' = bimap (+ fst location) (+ snd location) $ toIndex direction,
                     U.inRange b l',
                     let k' = k + m U.! l'
                 ]
        xs' = Q.union xs added
     in dijkstra minRun maxRun m end visited' xs'
  where
    b = U.bounds m

day17 :: IO ()
day17 = do
  -- input <- U.amap digitToInt . drawArray @U.UArray . lines <$> readFile "input/test17a.txt"
  -- input <- U.amap digitToInt . drawArray @U.UArray . lines <$> readFile "input/test17.txt"
  input <- U.amap digitToInt . drawArray @U.UArray . lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  putStrLn
    . ("day17a: " ++)
    . show
    $ dijkstra 0 3 input (snd $ U.bounds input) Set.empty (Q.singleton 0 (G (0, 0) East 0))
  putStrLn
    . ("day17b: " ++)
    . show
    $ dijkstra 4 10 input (snd $ U.bounds input) Set.empty (Q.fromList [(0, G (0, 0) East 0), (0, G (0, 0) South 0)])
