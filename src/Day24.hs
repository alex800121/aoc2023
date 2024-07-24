module Day24 where

import Data.Either (fromRight)
import Data.Matrix
import Data.Matrix qualified as Matrix
import Data.Maybe (isJust, mapMaybe)
import MyLib
import Paths_AOC2023
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

buildSolution :: (Fractional a) => Hail a -> Hail a -> ([[a]], [a])
buildSolution
  (Cons (x0, vx0) (Cons (y0, vy0) (Cons (z0, vz0) Nil)))
  (Cons (x1, vx1) (Cons (y1, vy1) (Cons (z1, vz1) Nil))) =
    ( [ [vy0 - vy1, vx1 - vx0, 0, y1 - y0, x0 - x1, 0],
        [0, vz0 - vz1, vy1 - vy0, 0, z1 - z0, y0 - y1]
      ],
      [ x0 * vy0 + y1 * vx1 - y0 * vx0 - x1 * vy1,
        y0 * vz0 + z1 * vy1 - z0 * vy0 - y1 * vz1
      ]
    )

-- day24b :: (Fractional a, Eq a) => [Hail a] -> Matrix a
day24b hs = invM * n
  where
    [x, y, z, w] = take 4 hs
    (a, b) = buildSolution x y
    (c, d) = buildSolution y z
    (e, f) = buildSolution z w
    m = Matrix.fromLists (a ++ c ++ e)
    n = Matrix.fromLists $ map (: []) $ b ++ d ++ f
    Right invM = Matrix.inverse m

day24 :: IO ()
day24 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
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
    . filter (\[x, y] -> maybe False (\(a, b) -> a >= minA && b >= minA && a <= maxA && b <= maxA) $ solveXY x y)
    $ pick 2 hails
  putStrLn
    . ("day24b: " ++)
    . show
    . round
    . sum
    . take 3
    . Matrix.toList
    $ day24b hails
