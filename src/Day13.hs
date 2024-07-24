module Day13 where

import Paths_AOC2023
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

getSym :: (Eq a) => [[a]] -> Maybe Int
getSym (x : xs) = f [x] xs
  where
    f _ [] = Nothing
    f ys zs'@(z : zs)
      | and (zipWith (==) ys zs') = Just $ length ys
      | otherwise = f (z : ys) zs

getSym' :: (Eq a) => [[a]] -> Maybe Int
getSym' (x : xs) = f [x] xs
  where
    f _ [] = Nothing
    f ys zs'@(z : zs)
      | 1 == length (filter not $ concat (zipWith (zipWith (==)) ys zs')) = Just $ length ys
      | otherwise = f (z : ys) zs

day13 :: IO ()
day13 = do
  input <- map lines . splitOn "\n\n" <$>(getDataDir >>= readFile . (++ "/input/input13.txt")) 
  let hs = mapMaybe getSym input
      vs = mapMaybe (getSym . transpose) input
      hs' = mapMaybe getSym' input
      vs' = mapMaybe (getSym' . transpose) input
  putStrLn
    .  ("day13a: " ++)
    . show
    $ sum hs * 100 + sum vs
  putStrLn
    .  ("day13b: " ++)
    . show
    $ sum hs' * 100 + sum vs'
