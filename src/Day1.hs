module Day1 where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (find, inits, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (isJust, mapMaybe)

digits =
  (`zip` ([1 .. 9] ++ [1 .. 9]))
    ( ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        ++ [[intToDigit x] | x <- [1 .. 9]]
    )

f :: String -> Int
f s = 10 * x + y
  where
    x = head . mapMaybe (fmap snd . (\x -> find (\(a, b) -> a `isPrefixOf` x) digits)) $ tails s
    y = head . mapMaybe (fmap snd . (\x -> find (\(a, b) -> a `isSuffixOf` x) digits)) $ reverse $ inits s

day1 :: IO ()
day1 = do
  -- input <- lines <$> readFile "input/test1.txt"
  input <- lines <$> readFile "input/input1.txt"
  let a = sum $ map (((+) <$> ((* 10) . head) <*> last) . map digitToInt . filter isDigit) input
      b = sum $ map f input
  putStrLn 
    . ("day1a: " ++)
    $ show a
  putStrLn 
    . ("day1b: " ++)
    $ show b
