module Day14 where

import Data.List (transpose, unfoldr)
import MyLib (firstCycle, firstRepeat, firstRepeat')
import Paths_AOC2023

rowRock :: String -> String
rowRock = f 0 0
  where
    f dotN oN [] = replicate dotN '.' <> replicate oN 'O'
    f dotN oN (x : xs) = case x of
      '.' -> f (dotN + 1) oN xs
      'O' -> f dotN (oN + 1) xs
      '#' -> replicate dotN '.' <> replicate oN 'O' <> "#" <> f 0 0 xs

day14a :: [String] -> Int
day14a = sum . zipWith (*) [1 ..] . map (length . filter (== 'O')) . reverse

turnRoll = map rowRock . transpose . reverse

cycleRoll :: [String] -> [String]
cycleRoll = (!! 4) . iterate turnRoll

day14 :: IO ()
day14 = do
  -- input <- lines <$> readFile "input/test14.txt"
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  let xs = iterate cycleRoll input
      Just (x, y, z) = firstCycle xs
      c = y - x
      n = (1000000000 - x) `mod` c
  putStrLn
    . ("day14a: " ++)
    . show
    . day14a
    . reverse
    . transpose
    $ turnRoll input
  putStrLn
    . ("day14b: " ++)
    . show
    . day14a
    . (!! n)
    $ iterate cycleRoll z

-- print $ xs !! (x + 100) == xs !! (y + 100)
