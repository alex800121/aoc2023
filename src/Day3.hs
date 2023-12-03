module Day3 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt, isNumber)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import MyLib

type Index = (Int, Int)

type Part = ([Index], Int)

type Gondola = (Index, Char)

getParts :: String -> ([Part], [Gondola])
getParts = f 0 0
  where
    f _ _ [] = ([], [])
    f x y ('\n' : s) = f 0 (y + 1) s
    f x y ('.' : s) = f (x + 1) y s
    f x y (i : s) | isNumber i = g [(x, y)] (x + 1) y (digitToInt i) s
    f x y (i : s) = second (((x, y), i) :) $ f (x + 1) y s
    g xy x y acc (i : s) | isNumber i = g ((x, y) : xy) (x + 1) y (10 * acc + digitToInt i) s
    g xy x y acc s = first ((xy, acc) :) $ f x y s

surroundings = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

day3a :: [Gondola] -> Part -> Maybe Gondola
day3a gondolas (xs, _) = find (\(x, _) -> any (`elem` map (bimap (+ fst x) (+ snd x)) surroundings) xs) gondolas

day3 :: IO ()
day3 = do
  -- (rawParts, gondolas) <- getParts <$> readFile "input/test3.txt"
  (rawParts, gondolas) <- getParts <$> readFile "input/input3.txt"
  let parts = filter (isJust . day3a gondolas) rawParts
      rawGears =
        sum
          . map product
          . filter ((== 2) . length)
          . Map.elems
          . Map.fromListWith (<>)
          $ mapMaybe
            ( \x -> case day3a gondolas x of
                Just (i, '*') -> Just (i, [snd x])
                _ -> Nothing
            )
            parts
  putStrLn
    . ("day3a: " ++)
    . show
    . sum
    . map snd
    $ parts
  putStrLn
    . ("day3a: " ++)
    . show
    $ rawGears
