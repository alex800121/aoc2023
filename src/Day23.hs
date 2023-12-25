module Day23 where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib

type M = Map Index (Map Index [Int])

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
buildM a end start = Map.singleton start $ f Set.empty initStarts 1 Map.empty
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
                    then Just i
                    else Nothing
          )
          [minBound .. maxBound]
    f visited starts i acc
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
        acc' = Map.unionWith (<>) (Map.fromSet (const [i + 2]) interest') $ Map.unionWith (<>) (Map.fromSet (const [i]) ended) acc

adjacent = [(0, 1), (0, -1), (-1, 0), (1, 0)]

day23 :: IO ()
day23 = do
  input <- drawArray @UArray . lines <$> readFile "input/input23.txt"
  return ()
