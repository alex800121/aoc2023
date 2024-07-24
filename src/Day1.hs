module Day1 where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (find, inits, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (isJust, mapMaybe)
import Paths_AOC2023

digitsA = [[intToDigit x] | x <- [1 .. 9]] `zip` [1 .. 9]

digitsB =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] `zip` ([1 .. 9] ++ [1 .. 9])

-- f :: String -> Int
f digits s = 10 * x + y
  where
    xy = mapMaybe (fmap snd . (\x -> find (\(a, b) -> a `isPrefixOf` x) digits)) $ tails s
    x = head xy
    y = last xy

day1 :: IO ()
day1 = do
  -- input <- lines <$> readFile "input/test1.txt"
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let a = sum $ map (f digitsA) input
      b = sum $ map (f (digitsA ++ digitsB)) input
  putStrLn
    . ("day1a: " ++)
    $ show a
  putStrLn
    . ("day1b: " ++)
    $ show b
