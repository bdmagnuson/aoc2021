module Aoc2021.Day25
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens hiding (Empty)
import Data.Attoparsec.Text qualified as P
import Data.List (transpose, unfoldr)

input = getInput "input/day25.txt" parser

data Spot = Empty | South | East deriving (Eq)

instance Show Spot where
  show Empty = "."
  show South = "v"
  show East = ">"

newtype Map = Map {unMap :: [[Spot]]} deriving (Eq)

instance Show Map where
  show = unlines . map concat . fmap (fmap show) . unMap

parser = Map <$> many (many p <* P.endOfLine)
  where
    p =
      P.char 'v' *> pure South
        <|> P.char '>' *> pure East
        <|> P.char '.' *> pure Empty

step = stepS . stepE

transposeM = Map . transpose . unMap

stepS = transposeM . stepD South . transposeM

stepE = stepD East

stepD :: Spot -> Map -> Map
stepD c (Map m) = Map (map go m)
  where
    go :: [Spot] -> [Spot]
    go v =
      let v' = (init . tail) $ f False ((last v) : v ++ [head v])
          f :: Bool -> [Spot] -> [Spot]
          f _ [] = []
          f _ (x : []) = [x]
          f True (Empty : xs) = c : (f False xs)
          f False (x : y : ys) =
            if x == c && y == Empty
              then Empty : (f True (y : ys))
              else x : (f False (y : ys))
       in v'

steps = unfoldr go
  where
    go m = let m' = step m in if m == m' then Nothing else Just (m, m')

part1 = length (steps input) + 1

part2 = 0
