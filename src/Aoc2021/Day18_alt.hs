module Aoc2021.Day18_alt
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.List (foldl1')

data Number a
  = Regular a
  | Snailfish (Number a) (Number a)
  deriving (Eq)

instance (Show a) => Show (Number a) where
  show (Regular a) = show a
  show (Snailfish a b) = "[" <> show a <> "," <> show b <> "]"

parser :: P.Parser [Number Int]
parser = many (pPair <* P.endOfLine)
  where
    pVal = (Regular . fromIntegral) <$> P.decimal
    pPair = do
      P.char '['
      left <- pVal <|> pPair
      P.char ','
      right <- pVal <|> pPair
      P.char ']'
      return $ Snailfish left right

input = getInput "input/day18.txt" parser

(<+>) :: Number Int -> Number Int -> Number Int
a <+> b = reduce $ Snailfish a b

reduce :: Number Int -> Number Int
reduce a = maybe a reduce (reduceExplode a <|> reduceSplit a)

reduceSplit :: Number Int -> Maybe (Number Int)
reduceSplit = walk Just
  where
    walk cc (Snailfish a b) =
      walk (\n -> cc $ Snailfish n b) a
        <|> walk (\n -> cc $ Snailfish a n) b
    walk cc (Regular x)
      | x >= 10 = cc $ split x
      | otherwise = Nothing
      where
        split x = let d = x `div` 2 in Snailfish (Regular d) (Regular (x - d))

reduceExplode :: Number Int -> Maybe (Number Int)
reduceExplode = walk 0 (\_ x _ -> Just x)
  where
    walk 4 explode (Snailfish (Regular a) (Regular b)) =
      explode a (Regular 0) b
    walk depth explode (Snailfish a b) =
      walk (depth + 1) explodeLeft a
        <|> walk (depth + 1) explodeRight b
      where
        explodeLeft x n y =
          explode x (Snailfish n (addToLeftMost b y)) 0
        explodeRight x n y =
          explode 0 (Snailfish (addToRightMost a x) n) y
    walk _ _ (Regular _) = Nothing

addToLeftMost :: Number Int -> Int -> Number Int
addToLeftMost a 0 = a
addToLeftMost a x = go a
  where
    go (Snailfish a b) = Snailfish (go a) b
    go (Regular y) = Regular (x + y)

addToRightMost :: Number Int -> Int -> Number Int
addToRightMost a 0 = a
addToRightMost a x = go a
  where
    go (Snailfish a b) = Snailfish a (go b)
    go (Regular y) = Regular (x + y)

magnitude :: Number Int -> Int
magnitude (Regular x) = x
magnitude (Snailfish a b) = magnitude a * 3 + magnitude b * 2

part1 :: [Number Int] -> Int
part1 = magnitude . foldl1' (<+>)

part2 :: [Number Int] -> Int
part2 inp =
  maximum
    [ magnitude (a <+> b)
      | a <- inp,
        b <- inp,
        a /= b
    ]
