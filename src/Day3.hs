module Day3 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt, isNumber)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib

type Index = (Int, Int)

type Part = (Set Index, Int)

type Gondola = Map Index Char

getParts :: String -> (Set Part, Gondola)
getParts = f 0 0
  where
    f _ _ [] = (Set.empty, Map.empty)
    f x y ('\n' : s) = f 0 (y + 1) s
    f x y ('.' : s) = f (x + 1) y s
    f x y (i : s) | isNumber i = g (Set.singleton (x, y)) (x + 1) y (digitToInt i) s
    f x y (i : s) = second (Map.insert (x, y) i) $ f (x + 1) y s
    g xy x y acc (i : s) | isNumber i = g (Set.insert (x, y) xy) (x + 1) y (10 * acc + digitToInt i) s
    g xy x y acc s = first (Set.insert (Set.unions (map (\(a, b) -> Set.map (bimap (+ a) (+ b)) xy) surroundings), acc)) $ f x y s

surroundings = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

day3 :: IO ()
day3 = do
  -- (rawParts, gondolas) <- getParts <$> readFile "input/test3.txt"
  (rawParts, gondolas) <- getParts <$> readFile "input/input3.txt"
  let parts = Set.filter (any (`Map.member` gondolas) . fst) rawParts
      gondolas' = Map.filter (== '*') gondolas
      rawGears =
        Map.map product
          . Map.filter ((== 2) . length)
          $ Map.mapWithKey
            ( \k _ ->
                map snd
                  . Set.toList
                  $ Set.filter (Set.member k . fst) parts
            )
            gondolas'
  putStrLn
    . ("day3a: " ++)
    . show
    . sum
    . map snd
    . Set.toList
    $ parts
  putStrLn
    . ("day3b: " ++)
    . show
    . sum
    $ rawGears
