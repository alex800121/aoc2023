module Day17 where

import qualified Data.Array.Unboxed as U
import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import qualified Data.PQueue.Prio.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib

data GameState = G
  { _location :: Index,
    _direction :: Direction,
    _moves :: Int
  }
  deriving (Show, Ord, Eq)

type Index = (Int, Int)

type Q = Q.MinPQueue Int GameState

type M = U.UArray Index Int

dijkstra :: M -> Index -> Set GameState -> Q -> Maybe Int
dijkstra m end visited q = case q of
  Q.Empty -> Nothing
  (k, g) Q.:< _ | _location g == end -> Just k
  (_, g) Q.:< xs | g `Set.member` visited -> dijkstra m end visited xs
  (k, g@(G location direction moves)) Q.:< xs ->
    let visited' = Set.insert g visited
        added =
          Q.fromList $
            [ (k', G l' d' m')
              | let m' = 1,
                d' <- map ($ direction) [succ, pred],
                let l' = bimap (+ fst location) (+ snd location) $ toIndex d',
                U.inRange b l',
                let k' = k + m U.! l'
            ] ++
            [ (k', G l' direction m')
              | let m' = moves + 1,
                m' <= 3,
                let l' = bimap (+ fst location) (+ snd location) $ toIndex direction,
                U.inRange b l',
                let k' = k + m U.! l'
            ]
        xs' = Q.union xs added
     in dijkstra m end visited' xs'
  where
    b = U.bounds m

day17 :: IO ()
day17 = do
  -- input <- U.amap digitToInt . drawArray @U.UArray . lines <$> readFile "input/test17.txt"
  input <- U.amap digitToInt . drawArray @U.UArray . lines <$> readFile "input/input17.txt"
  print $ dijkstra input (snd $ U.bounds input) Set.empty (Q.singleton 0 (G (0, 0) East 0))
