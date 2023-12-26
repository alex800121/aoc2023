module Day24 where

import Data.Either (fromRight)
import Data.Matrix
import qualified Data.Matrix as Matrix
import Data.Maybe (isJust, mapMaybe)
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

type S3 = S (S (S Z))

type Hail a = Vec S3 (a, a)

type Range a = Vec S3 (a, a)

parseInput :: (Fractional a) => Parser (Hail a)
parseInput = do
  a <- toVec (SS (SS (SS SZ))) . map fromIntegral <$> sepBy signedInteger (char ',' <* space)
  space >> char '@' <* space
  b <- toVec (SS (SS (SS SZ))) . map fromIntegral <$> sepBy signedInteger (char ',' <* space)
  return $ vZipWith (,) a b

solveXY :: (Eq a, Fractional a, Ord a) => Hail a -> Hail a -> Maybe (a, a)
solveXY (Cons (x0, tx0) (Cons (y0, ty0) _)) (Cons (x1, tx1) (Cons (y1, ty1) _)) = case Matrix.toList <$> ((*) <$> invL <*> pure r) of
  Right [t0, t1] | t0 >= 0 && t1 >= 0 -> Just (x0 + tx0 * t0, y0 + ty0 * t0)
  _ -> Nothing
  where
    l = Matrix.fromLists [[tx0, -tx1], [ty0, -ty1]]
    r = Matrix.fromLists [[x1 - x0], [y1 - y0]]
    invL = Matrix.inverse l

day24 :: IO ()
day24 = do
  input <- lines <$> readFile "input/input24.txt"
  -- input <- lines <$> readFile "input/test24.txt"
  let hails = mapMaybe (parseMaybe (parseInput @Rational)) input
      minA = 200000000000000
      maxA = 400000000000000
  -- minA = 7
  -- maxA = 27
  putStrLn
    . ("day24a: " ++)
    . show
    . length
    . filter id
    . map (\[x, y] -> maybe False (\(a, b) -> a >= minA && b >= minA && a <= maxA && b <= maxA) $ solveXY x y)
    $ pick 2 hails
