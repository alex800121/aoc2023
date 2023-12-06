module Day6 where
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (splitOn)

f :: (Floating a) => a -> a -> a -> (a -> a -> a) -> a
f a b c x = (negate b `x` sqrt (b ^ 2 - (4 * a * c))) / (2 * a)

g (x, y) = floor (f' (-)) - ceiling (f' (+)) + 1
  where
   f' = f (-1) x (-y)

testInput = [(7, 9), (15, 40), (30, 200)]
-- x * (7 - x)
-- = -(x^2) + 7x
day6 :: IO ()
day6 = do
  input <- lines  <$> readFile "input/input6.txt"
  let day6a = product $ map g . (\(x : y : _) -> zip x y) . map (map read . tail . words) $ input
      input' = (\(x : y : _) -> g (read x, read y)) $ map (filter (/= ' ') . last . splitOn ":") input
      -- day6b = g input'
  putStrLn
    . ("day6a: " ++)
    . show
    $ day6a
  putStrLn 
    . ("day6b: " ++)
    . show
    $ input'
