{-# LANGUAGE PatternSynonyms #-}

module Day20 where

import Optics
import Control.Monad (foldM, when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Bits (Bits (..))
import Data.Char (isAlpha)
import Data.List (partition, sort, foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector.Strict qualified as SV
import Data.Vector.Strict.Mutable qualified as SMV
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as M
import Data.Word
import Debug.Trace
import Paths_AOC2023
import Queue.Ephemeral (EphemeralQueue)
import Queue.Ephemeral qualified as Q

targets = ["js", "zb", "rr", "bs"]

-- Vector (Word, Word, Int, Int)
-- flip    send  state low  high
-- conj    send  from  low  high
-- flip state : on -> 1, off -> 0
-- conj "from" state : low -> 1, high -> 0
-- vector[0]: broadcaster
-- vector[1]: rx
-- vector[2..(1 + isFF)]: flipflops
-- vector[(2 + isFF)..]: conj

type Signal = (Int, Int, Bool)

type State = (Word, Int, Int)

pattern Low = True

pattern High = False

readSend :: Word -> EphemeralQueue Int
readSend = f . (0,)
  where
    f (i, x)
      | x == 0 = Q.empty
      | m == 0 = f (succ i, d)
      | otherwise = Q.enqueue i (f (succ i, d))
      where
        (d, m) = x `divMod` 2

solveA :: Int -> SV.Vector (EphemeralQueue Int) -> Vector State -> Int
solveA ffLen ref v = runST $ do
  mv <- V.thaw v
  mapM_ (\_ -> f mv (Q.singleton (0, 0, Low))) [0 .. 999]
  (low, high) <- M.foldl' (\(a, b) (d, low, high) -> (a + low, b + high)) (0, 0) mv
  pure (low * high)
  where
    f :: STVector s State -> EphemeralQueue Signal -> ST s ()
    f mv Q.Empty = pure ()
    f mv (Q.Full s ss) = f mv . (ss <>) =<< sendSignal s ffLen ref mv

solveB :: Int -> SV.Vector (EphemeralQueue Int) -> Vector State -> Word -> Int
solveB ffLen ref v iTargets = runST $ do
  mv <- V.thaw v
  f 1 1 iTargets mv button
  where
    button = Q.singleton (0, 0, Low)
    f :: Int -> Int -> Word -> STVector s State -> EphemeralQueue Signal -> ST s Int
    f i acc t mv q
      | 0 == t = pure acc
      | Q.Empty <- q = f (succ i) acc t mv button
      | Q.Full s@(origin, to, sig) ss <- q,
        (acc', t') <- if t `testBit` origin && not sig then (lcm acc i, t `clearBit` origin) else (acc, t) =
          f i acc' t' mv . (ss <>) =<< sendSignal s ffLen ref mv

sendSignal :: Signal -> Int -> SV.Vector (EphemeralQueue Int) -> STVector s State -> ST s (EphemeralQueue Signal)
sendSignal (origin, to, signal) ffLen ref v
  | to == 0 = do
      M.modify v (over _2 succ) 0
      pure $ Q.map (to,,Low) $ ref SV.! 0
  | to == 1 = M.modify v (over (if signal then _2 else _3) succ) 1 >> pure Q.empty
  | isConj ffLen to = do
      (state, low, high) <- M.read v to
      let state' = if signal then state `setBit` origin else state `clearBit` origin
          sendSig = state' == 0
      M.modify v (set _1 state' . over (if signal then _2 else _3) succ) to
      pure (Q.map (to,,sendSig) (ref SV.! to))
  | otherwise = do
      (state, low, high) <- M.read v to
      if signal
        then do
          M.write v to (state `complementBit` 0, succ low, high)
          pure (Q.map (to,,state `testBit` 0) (ref SV.! to))
        else M.modify v (over _3 succ) to >> pure Q.empty

readInput :: String -> (Map.Map String Int, (Int, (SV.Vector (EphemeralQueue Int), Vector State)))
readInput s =
  ( keyMap,
    ( ffLen,
      runST $ do
        ref <- SMV.replicate l Q.empty
        v <- M.replicate l (0, 0, 0)
        mapM_
          ( \s -> do
              let (name, send) = f s
              mapM_
                ( \t -> do
                    SMV.modify ref (Q.enqueue t) name
                    when (isConj ffLen t) (M.modify v (over _1 (`setBit` name)) t)
                )
                send
          )
          ss
        (,) <$> SV.freeze ref <*> V.freeze v
    )
  )
  where
    ss = map words $ sort $ lines s
    keyMap = Map.fromList $ zip ("broadcaster" : "rx" : map (filter isAlpha . head) (init ss)) [0 ..]
    l = Map.size keyMap
    ffLen = length $ takeWhile ((== '%') . head . head) ss
    f (name : _ : send) = (keyMap Map.! filter isAlpha name, mapMaybe ((keyMap Map.!?) . filter isAlpha) send)

isConj ffLen x = x >= 2 + ffLen

day20 :: IO ()
day20 = do
  (keyMap, (ffLen, (ref, state))) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let iTargets = foldl' setBit 0 $ map (keyMap Map.!) targets
  putStrLn
    . ("day20a: " ++)
    . show
    $ solveA ffLen ref state
  putStrLn
    . ("day20b: " ++)
    . show
    $ solveB ffLen ref state iTargets
