module Aoc2021.Day18
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Either (fromRight)
import Debug.Trace

data Node a b
  = Value b
  | Pair a (Node a b) (Node a b)
  deriving (Functor)

instance (Show b) => Show (Node a b) where
  show (Value b) = show b
  show (Pair _ a b) = "[" ++ show a ++ "," ++ show b ++ "]"

type Ann = Node (Integer, Integer) Integer

parser = many (pPair <* P.endOfLine)
  where
    pVal = Value <$> P.decimal
    pPair = do
      P.char '['
      left <- pVal <|> pPair
      P.char ','
      right <- pVal <|> pPair
      P.char ']'
      return $ Pair () left right

input = getInput "input/day18.txt" parser

-- annotate :: Node a b -> Node (Int, Int) b
annotate (Value v) = Value v
annotate (Pair _ (Value l) (Value r)) = Pair (1, 1) (Value l) (Value r)
annotate (Pair _ (Value v) p) = let p'@(Pair (l, r) _ _) = annotate p in Pair (1, l + r) (Value v) p'
annotate (Pair _ p (Value v)) = let p'@(Pair (l, r) _ _) = annotate p in Pair (l + r, 1) p' (Value v)
annotate (Pair _ p1 p2) =
  let p1'@(Pair (l1, r1) _ _) = annotate p1
      p2'@(Pair (l2, r2) _ _) = annotate p2
   in Pair (l1 + r1, l2 + r2) p1' p2'

unannotate :: Node a b -> Node () b
unannotate (Value b) = Value b
unannotate (Pair _ l r) = Pair () (unannotate l) (unannotate r)

findExplode = go 0 0
  where
    go :: Integer -> Integer -> Ann -> Maybe (Integer, Integer, Integer)
    go _ _ (Value _) = Nothing
    go 4 i (Pair _ (Value l) (Value r)) = Just (i, l, r)
    go d i (Pair (l, r) n1 n2) =
      case (go (d + 1) i n1, go (d + 1) (i + l) n2) of
        (Just n, _) -> Just n
        (Nothing, Just n) -> Just n
        (Nothing, Nothing) -> Nothing

findSplit = go 0
  where
    go :: Integer -> Ann -> Maybe Integer
    go i (Value n) = if n > 9 then Just i else Nothing
    go i (Pair (l, r) n1 n2) =
      case (go i n1, go (i + l) n2) of
        (Just n, _) -> Just n
        (Nothing, Just n) -> Just n
        (Nothing, Nothing) -> Nothing

magnitude :: Node a Integer -> Integer
magnitude (Value a) = a
magnitude (Pair _ a b) = 3 * magnitude a + 2 * magnitude b

add :: Node () b -> Node () b -> Node () b
add a b = Pair () a b

reduce :: Node () Integer -> Node () Integer
reduce x =
  case findExplode ann of
    Nothing ->
      case findSplit ann of
        Nothing -> x
        Just i -> reduce (unannotate $ splitNode i ann)
    Just (i, l, r) -> reduce (unannotate . zeroNode i . addIndex (i - 1) l . addIndex (i + 2) r $ ann)
  where
    ann = annotate x
    addIndex :: Integer -> Integer -> Ann -> Ann
    addIndex 0 a (Value v) = Value (v + a)
    addIndex i a (Pair (l, r) n1 n2)
      | i < 0 = Pair (l, r) n1 n2
      | i > (l + r - 1) = Pair (l, r) n1 n2
      | i < l = Pair (l, r) (addIndex i a n1) n2
      | otherwise = Pair (l, r) n1 (addIndex (i - l) a n2)

    splitNode :: Integer -> Ann -> Ann
    splitNode 0 (Value v) = let l = v `div` 2 in Pair (0, 0) (Value l) (Value (v - l))
    splitNode i (Pair (l, r) n1 n2)
      | i < l = Pair (l, r) (splitNode i n1) n2
      | otherwise = Pair (l, r) n1 (splitNode (i - l) n2)

zeroNode = go 0
  where
    go :: Integer -> Integer -> Ann -> Ann
    go 4 _ _ = Value 0
    go d i (Pair (l, r) n1 n2)
      | i < l = Pair (l, r) (go (d + 1) i n1) n2
      | otherwise = Pair (l, r) n1 (go (d + 1) (i - l) n2)

solve x = magnitude $ foldl1 (\x y -> reduce (add x y)) x

part1 = solve input

part2 = maximum $ concatMap f (combinations 2 input)
  where
    f [x, y] = [magnitude $ reduce (add x y), magnitude $ reduce (add y x)]
