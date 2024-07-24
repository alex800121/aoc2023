module Day19 where

import Control.Lens
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Debug.Trace
import MyLib hiding (S)
import MyLib qualified as M
import Paths_AOC2023
import Text.Megaparsec
import Text.Megaparsec.Char

data XMAS a = XMAS {_getX :: a, _getM :: a, _getA :: a, _getS :: a}
  deriving (Show, Eq, Ord)

type XMASI = XMAS Int

type XMASR = Vec (M.S (M.S (M.S (M.S Z)))) Range

type Range = (Int, Int)

data Field = X | M | A | S deriving (Show, Eq, Ord, Enum)

defaultXMAS = XMAS 0 0 0 0

data Clause = C Field Ordering Int Dest | D Dest deriving (Show, Eq, Ord)

data Dest = Accept | Reject | Next {_getNext :: String} deriving (Show, Eq, Ord)

type Rule = [Clause]

type Rules = Map String Rule

defaultXMASR :: XMASR
defaultXMASR = pure (1, 4001)

applyRuleR :: Rule -> XMASR -> Map Dest [XMASR]
applyRuleR = f Map.empty
  where
    g x y = jointEucVecs (x <> y)
    -- f x y z | trace (show z) False = undefined
    -- f acc [] _ = acc
    f acc [D d] v = Map.insertWith g d [v] acc
    f acc (C fi o i d : rs) v = case v1 of
      Just v' -> f acc' rs v'
      Nothing -> acc'
      -- Nothing -> trace ("done: " ++ show acc') acc'
      where
        acc' = Map.insertWith g d v0 acc
        (v0, v1) = case o of
          GT ->
            ( maybeToList $ overlapEucVec v (writeFieldR defaultXMASR fi (i + 1, 4001)),
              overlapEucVec v (writeFieldR defaultXMASR fi (1, i + 1))
            )
          LT ->
            ( maybeToList $ overlapEucVec v (writeFieldR defaultXMASR fi (1, i)),
              overlapEucVec v (writeFieldR defaultXMASR fi (i, 4001))
            )

applyRulesR :: Rules -> Map Dest [XMASR] -> Map Dest [XMASR]
applyRulesR r acc
  -- \| trace (show (Map.filter ((> 1) . length) acc) ++ "\n") False = undefined
  -- \| acc' == Map.filter (not . null) (Map.map jointEucVecs acc'') = undefined
  -- \| traceShow (Map.map calc acc) False = undefined
  | all (`elem` [Accept, Reject]) (Map.keys acc') = acc'
  | otherwise = applyRulesR r acc''
  where
    -- calc = sum . product . map (fmap (uncurry subtract))
    g x y = x ++ y
    acc' = Map.filter (not . null) acc
    acc'' =
      Map.foldlWithKey'
        ( \b k a -> case k of
            Next s ->
              let rule = r Map.! s
                  xs = map (applyRuleR rule) a
               in Map.unionsWith g (b : xs)
            _ -> Map.insertWith g k a b
        )
        Map.empty
        acc'

readFieldR :: (Enum x) => Vec n a -> x -> a
readFieldR v = vRead v . fromEnum

writeFieldR :: (Enum x) => Vec n a -> x -> a -> Vec n a
writeFieldR v x = vWrite v (fromEnum x)

modifyFieldR :: (Enum x) => Vec n a -> x -> (a -> a) -> Vec n a
modifyFieldR v x = vModify v (fromEnum x)

applyRule :: Rule -> XMASI -> Dest
applyRule [] _ = undefined
applyRule (D d : _) _ = d
applyRule (C f o i d : rs) xmas =
  if compare (readField f xmas) i == o then d else applyRule rs xmas

applyRules :: Rules -> Dest -> XMASI -> Dest
applyRules m (Next s) xmas = applyRules m (applyRule (m Map.! s) xmas) xmas
applyRules _ d _ = d

readField X = _getX
readField M = _getM
readField A = _getA
readField S = _getS

writeField X x m = m {_getX = x}
writeField M x m = m {_getM = x}
writeField A x m = m {_getA = x}
writeField S x m = m {_getS = x}

parseRules :: Parser Rules
parseRules = do
  name <- many letterChar
  rules <-
    between
      (char '{')
      (char '}')
      (sepBy parseClause (char ','))
  pure $ Map.singleton name rules

parseClause :: Parser Clause
parseClause = do
  let f = do
        x <-
          (char 'x' >> pure X)
            <|> (char 'm' >> pure M)
            <|> (char 'a' >> pure A)
            <|> (char 's' >> pure S)
        b <- (char '>' >> pure GT) <|> (char '<' >> pure LT)
        i <- signedInteger
        char ':'
        pure (x, b, i)
  a <- optional (try f)
  d <- (char 'A' >> pure Accept) <|> (char 'R' >> pure Reject) <|> Next <$> many letterChar
  case a of
    Just (x, b, i) -> pure $ C x b i d
    Nothing -> pure $ D d

parseXMAS :: Parser XMASI
parseXMAS =
  foldl' (&) defaultXMAS
    <$> between
      (char '{')
      (char '}')
      ( sepBy
          ( parseXMASLens
              >>= \f ->
                char '='
                  >> signedInteger
                  >>= \x -> pure (writeField f x)
          )
          (char ',')
      )

parseXMASLens :: Parser Field
parseXMASLens =
  (char 'x' >> pure X)
    <|> (char 'm' >> pure M)
    <|> (char 'a' >> pure A)
    <|> (char 's' >> pure S)

calcXMAS :: XMASI -> Int
calcXMAS x = sum $ map ($ x) [_getX, _getM, _getA, _getS]

day19 :: IO ()
day19 = do
  -- x : y : _ <- map lines . splitOn "\n\n" <$> readFile "input/test19.txt"
  x : y : _ <- map lines . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  let rules = Map.unions $ mapMaybe (parseMaybe parseRules) x
      xmas = mapMaybe (parseMaybe parseXMAS) y
  putStrLn
    . ("day19a: " ++)
    . show
    . sum
    . map calcXMAS
    $ filter ((== Accept) . applyRules rules (Next "in")) xmas
  putStrLn
    . ("day19b: " ++)
    . show
    . sum
    . map (product . fmap (uncurry subtract))
    . (Map.! Accept)
    . applyRulesR rules
    $ Map.singleton (Next "in") [defaultXMASR]
