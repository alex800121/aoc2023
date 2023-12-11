{-# LANGUAGE LambdaCase #-}

module Day11 where

import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import MyLib (drawMap, pick)

type Index = (Int, Int)

manhattan :: (Num a) => (a, a) -> (a, a) -> a
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

expandLine :: Int -> (String -> Bool) -> [String] -> [String]
expandLine _ _ [] = []
expandLine n f (x : xs) = if f x then replicate n x ++ expandLine n f xs else x : expandLine n f xs

day11 :: IO ()
day11 = do
  input <- lines <$> readFile "input/input11.txt"
  -- input <- lines <$> readFile "input/test11.txt"
  let expandF = expandLine 2 (all (== '.'))
      expandF3 = expandLine 3 (all (== '.'))
      expanded = transpose . expandF . transpose $ expandF input
      expanded3 = transpose . expandF3 . transpose $ expandF3 input
      f = sum . map (\(x : y : _) -> manhattan x y) . pick 2
      galaxies = Map.keys $ drawMap (\case '#' -> Just (); _ -> Nothing) expanded
      galaxies3 = Map.keys $ drawMap (\case '#' -> Just (); _ -> Nothing) expanded3
      day11a = f galaxies
      diff = f galaxies3 - f galaxies
  putStrLn
    . ("day11a: " ++)
    . show
    $ day11a
  putStrLn
    . ("day11b: " ++)
    . show
    $ day11a + ((1000000 - 2) * diff)
