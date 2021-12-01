module Day01
  ( part1
  , part2
  ) where

import AocUtils

import qualified Data.Attoparsec.Text as P
import Control.Applicative

parser = many (P.decimal <* P.endOfLine)

input = getInput "input/day01.txt" parser

do_part1 input = sum increased
  where
    increased = map f pairs
      where f (Nothing, _) = 0
            f (_, Nothing) = 0
            f (a, b)       = if a < b then 1 else 0

    pairs = let l = fmap pure input in zip (Nothing:l) (l ++ [Nothing])

triple l@(a:b:c:xs) = (a + b + c):(triple (tail l))
triple _ = []

part1 = do_part1 input
part2 = do_part1 (triple input)


