module Day15 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Char (chr, isAlpha, ord)
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.List (deleteBy, findIndex, foldl')
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

day15a :: String -> Int
day15a = foldl' (\acc x -> ((acc + ord x) * 17) `mod` 256) 0

type HMap s = M.STVector s [(String, Int)]

readIns :: String -> HMap s -> ST s ()
readIns s hm
  | '=' `elem` s = do
      xs <- M.read hm h
      M.modify hm (f (x, focal)) h
  | otherwise = M.modify hm (deleteBy ((==) `on` fst) (x', 0)) h'
  where
    x : y : _ = splitOn "=" s
    x' = init s
    focal = read @Int y
    h = day15a x
    h' = day15a x'
    f a [] = [a]
    f (a, ai) ((b, bi) : bs) = if a == b then (a, ai) : bs else (b, bi) : f (a, ai) bs

day15b :: [String] -> V.Vector [(String, Int)]
day15b s = runST $ do
  v <- M.replicate 265 []
  foldlM (\_ x -> readIns x v) () s
  V.freeze v

calc :: [(a, Int)] -> Int
calc = sum . zipWith (*) [1 ..] . map snd

day15 :: IO ()
day15 = do
  input <- splitOn "," . init <$> readFile "input/input15.txt"
  putStrLn
    . ("day15a: " ++)
    . show
    . sum
    $ map day15a input
  putStrLn
    . ("day15b: " ++)
    . show
    . sum
    . V.imap (\i x -> calc x * (i + 1))
    . day15b
    $ input

