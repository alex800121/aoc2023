module Day9 where

import Data.List (scanl1)
import Debug.Trace (traceShow)

{-
10  13  16  21  30  45  68  -> f3 n = C3 + (map f2 [0..n]) = C3 + (C2 * n) + (C1 * (n - 1) * n / 2) + (C0 * (n - 2) * (n - 1) * n / 3!), C3 = 10, C2 = 3, C1 = 0, C0 = 2
   3   3   5   9  15  23    -> f2 n = C2 + (map f1 [0..n]) = C2 + (C1 * n) + (C0 * (n - 1) * n / 2!) , C2 = 3, C1 = 0, C0 = 2
     0   2   4   6   8      -> f1 n = C1 + n * (f0 n) = C1 + (C0 * n / 1!), C1 = 0, C0 = 2
       2   2   2   2        -> f0 n = C0, C0 = 2
         0   0   0          -> 0
-}

factorial :: (Integral a) => a -> a
factorial n
  | n <= 0 = 1
  | otherwise = n * factorial (n - 1)

buildFormula :: [Int] -> Int -> Int
buildFormula = f []
  where
    f acc xs
      -- \| traceShow acc False = undefined
      | all (== 0) xs = \n -> sum $ zipWith (*) (reverse acc) [ys | z <- [0 ..], let fz = factorial z, let zs = [1 .. z], let ys = product (map (`subtract` (n + 1)) zs) `div` fz]
      | otherwise = f (head xs : acc) (sub xs)

sub :: [Int] -> [Int]
sub (x : y : xs) = (y - x) : sub (y : xs)
sub _ = []

test = buildFormula [10, 13, 16, 21, 30, 45, 68]

day9a :: [Int] -> Int -> Int
day9a = buildFormula

-- test = scanl1 subtract [10, 13, 16, 21, 30, 45, 68]

day9 :: IO ()
day9 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input/input9.txt"
  putStrLn
    . ("day9a: " ++)
    . show
    . sum
    $ map (\x -> buildFormula x (length x)) input
  putStrLn
    . ("day9b: " ++)
    . show
    . sum
    $ map (\x -> buildFormula x (-1)) input
