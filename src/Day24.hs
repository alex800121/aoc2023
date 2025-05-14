module Day24 where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Either (fromRight)
import Data.Matrix
import Data.Matrix qualified as Matrix
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid (Sum (..))
import MyLib
import Paths_AOC2023
import Text.Megaparsec
import Text.Megaparsec.Char

-- type S3 = S (S (S Z))
-- type Hail a = Vec S3 (a, a)
-- type Range a = Vec S3 (a, a)
type P a = (a, a)

type Hail a = (P a, P a, P a)

type Range a = (P a, P a, P a)

parseInput :: Parser (Hail Rational)
parseInput = do
  [a0, a1, a2] <- map fromIntegral <$> sepBy signedInteger (char ',' <* space)
  space >> char '@' <* space
  [b0, b1, b2] <- map fromIntegral <$> sepBy signedInteger (char ',' <* space)
  return ((a0, b0), (a1, b1), (a2, b2))

-- x0 + vx0 * t0 = x1 + vx1 * t1
-- y0 + vy0 * t0 = y1 + vy1 * t1
-- vx0 t0 - vx1 t1 = x1 - x0 = x'
-- vy0 t0 - vy1 t1 = y1 - y0 = y'
-- (vx0 vy1 - vy0 vx1) t0 = (x' vy1 - y' vx1)
-- (vx1 vy0 - vy1 vx0) t1 = (y' vx0 - x' vy0)
solveXY :: Rational -> Rational -> Hail Rational -> Hail Rational -> Bool
solveXY minR maxR ((x0, vx0), (y0, vy0), _) ((x1, vx1), (y1, vy1), _) =
  d0 /= 0
    && d1 /= 0
    && t0 >= 0
    && t1 >= 0
    && a <= maxR
    && b <= maxR
    && a >= minR
    && b >= minR
  where
    x' = x1 - x0
    y' = y1 - y0
    d0 = vx0 * vy1 - vy0 * vx1
    d1 = vx1 * vy0 - vy1 * vx0
    a0 = x' * vy1 - y' * vx1
    a1 = y' * vx0 - x' * vy0
    t0 = a0 / d0
    t1 = a1 / d1
    a = x0 + vx0 * t0
    b = y0 + vy0 * t0

buildSolution :: Hail Rational -> Hail Rational -> ([[Rational]], [Rational])
buildSolution
  ((x0, vx0), (y0, vy0), (z0, vz0))
  ((x1, vx1), (y1, vy1), (z1, vz1)) =
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
    [(a, b), (c, d), (e, f)] = parMap rpar (uncurry buildSolution) [(x, y), (y, z), (z, w)]
    m = Matrix.fromLists (a ++ c ++ e)
    n = Matrix.fromLists $ map (: []) $ b ++ d ++ f
    Right invM = Matrix.inverse m

day24 :: IO ()
day24 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  -- input <- lines <$> readFile "input/test24.txt"
  let hails = mapMaybe (parseMaybe parseInput) input
      minA = 200000000000000
      maxA = 400000000000000
      !xs = pick 2 hails
  -- minA = 7
  -- maxA = 27
  putStrLn
    . ("day24a: " ++)
    . show
    -- . mconcat
    . length
    -- . filter id
    . filter (\[x, y] -> solveXY minA maxA x y)
    $ xs
  putStrLn
    . ("day24b: " ++)
    . show
    . round
    . sum
    . take 3
    . Matrix.toList
    $ day24b hails
