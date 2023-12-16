module Day12 where

import Control.Monad.Trans.State.Strict (State, evalState, get, modify, modify')
import qualified Data.Array as A
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.List (inits, intercalate, tails, transpose, uncons)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

inputParser :: String -> (String, [Int])
inputParser s = (x, i)
  where
    x : y : _ = words s
    i = map read $ splitOn "," y

type Cache = A.Array (Int, Int) Int

calc :: String -> [Int] -> Int
calc x y = calc' x y
  where
    lx = length x
    ly = length y
    calc' a b = cache A.! (lx - length a, ly - length b)
    cache =
      A.array
        ((0, 0), (length x, length y))
        [ ((a, b), n)
          | a <- [0 .. lx],
            b <- [0 .. ly],
            let n = calc'' (drop a x) (drop b y)
        ]
    calc'' s [] = if '#' `elem` s then 0 else 1
    calc'' [] (_ : _) = 0
    calc'' ss'@(s : ss) ns'@(n : ns) = case s of
      '.' -> calc' ss ns'
      '#' ->
        let (a, b) = splitAt n ss'
         in if length a == n && '.' `notElem` a && maybe True ((/= '#') . fst) (uncons b)
              then calc' (drop 1 b) ns
              else 0
      '?' -> calc' ss ns' + calc'' ('#' : ss) ns'

day12 :: IO ()
day12 = do
  input <- map inputParser . lines <$> readFile "input/input12.txt"
  -- input <- map inputParser . lines <$> readFile "input/test12.txt"
  putStrLn
    . ("day12a: " ++)
    . show
    . sum
    $ map (uncurry calc) input
  putStrLn
    . ("day12b: " ++)
    . show
    . sum
    $ map (\(x, y) -> calc (intercalate "?" $ replicate 5 x) (concat $ replicate 5 y)) input
