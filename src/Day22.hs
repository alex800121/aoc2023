module Day22 where

import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import MyLib
import Text.Megaparsec (parseMaybe, parseTest, sepBy)
import Text.Megaparsec.Char
import Control.Monad.Trans.State (State, get, evalState)
import Data.List (partition)
import Control.Monad (foldM)

type Brick = Vec (S (S (S Z))) (Int, Int)

data GameState = G
  { _stabilized :: Map Brick (Set Brick),
    _depend :: Map Brick (Set Brick),
    _free :: Set Brick
  }
  deriving (Show, Eq, Ord)

parseBlock :: Parser Brick
parseBlock = do
  a <- sepBy signedInteger (char ',')
  char '~'
  b <- sepBy signedInteger (char ',')
  return $ toVec (SS (SS (SS SZ))) $ reverse $ map (second (+ 1)) $ zipWith (\x y -> (min x y, max x y)) a b

runG :: Int -> GameState -> GameState
runG floor g = case Set.minView (_free g) of
  Nothing -> g
  Just (x, xs) | traceShow (length (_free g), length (_stabilized g)) True -> runG floor (dropFree floor x (g {_free = xs}))

dropFree :: Int -> Brick -> GameState -> GameState
dropFree floor b g
  | fst (vRead b' 0) <= floor =
      g
        { _stabilized = stabilized,
          _depend = Map.insert b (Set.singleton $ pure (0, 0)) $ _depend g
        }
  | not $ Set.null depend =
      g'
        { _stabilized =
            Set.foldl'
              ( \acc x ->
                  Map.insertWith
                    Set.union
                    x
                    (Set.singleton b)
                    acc
              )
              stabilized
              depend
        }
  | otherwise = dropFree floor b' g
  where
    b' = vModify b 0 (bimap (subtract 1) (subtract 1))
    depend = Set.filter (isJust . overlapEucVec b') (Map.keysSet $ _stabilized g)
    stabilized = Map.insert b Set.empty (_stabilized g)
    g' = g {_depend = Map.insert b depend (_depend g)}

removable :: GameState -> Brick -> Bool
removable g b = all ((> 1) . length . (_depend g Map.!)) (_stabilized g Map.! b)

disintegrate :: GameState -> Brick -> State (Map Brick (Set Brick)) (Set Brick)
disintegrate g b = get >>= \m -> maybe f return (m Map.!? b)
  where
    g' = g {_depend = Map.map (Set.filter (/= b)) $ _depend g}
    f = do
      let x = Set.filter (null . (_depend g' Map.!)) (_stabilized g Map.! b)
          g'' = g' {_depend = Map.map (Set.filter (`Set.notMember` x)) $ _depend g'}
      s <- foldM (\acc b' -> (acc <>) <$> disintegrate g'' b') Set.empty x
      return (x <> s)

day22 :: IO ()
day22 = do
  input <- lines <$> readFile "input/test22.txt"
  -- input <- lines <$> readFile "input/input22.txt"
  let initG = G Map.empty Map.empty (Set.fromList (mapMaybe (parseMaybe parseBlock) input))
      g = runG 0 initG
      (day22a, day22b) = partition (removable g) (Map.keys (_stabilized g))
      n = map length $ evalState (mapM (disintegrate g) day22b) Map.empty
  print $ length day22a
  print $ sum n
