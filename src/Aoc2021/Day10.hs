module Aoc2021.Day10
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.List (sort)

input = getInput "input/day10.txt" parser

parser = many (many (P.satisfy (P.inClass "[]{}()<>")) <* P.endOfLine)

data Status
  = Valid
  | Invalid Int
  | Incomplete Int
  deriving (Show)

$(makePrisms ''Status)

scores = map (go []) input
  where
    go [] [] = Valid
    go xs [] = Incomplete (foldl f 0 xs)
      where
        f a c = a * 5 + (s c)
        s = \case
          '(' -> 1
          '[' -> 2
          '{' -> 3
          '<' -> 4
    go [] (x : xs)
      | x `elem` ("[(<{" :: String) = go [x] xs
      | otherwise =
          case x of
            ']' -> Invalid 57
            ')' -> Invalid 3
            '>' -> Invalid 25137
            '}' -> Invalid 1197
    go s@(s' : ss) (x : xs)
      | x `elem` ("[(<{" :: String) = go (x : s) xs
      | otherwise =
          case x of
            ']' -> if s' == '[' then go ss xs else Invalid 57
            ')' -> if s' == '(' then go ss xs else Invalid 3
            '>' -> if s' == '<' then go ss xs else Invalid 25137
            '}' -> if s' == '{' then go ss xs else Invalid 1197

getScores :: Prism' Status Int -> [Status] -> [Int]
getScores s l = l ^.. folded . filtered (has s) . s

part1 = (sum . getScores _Invalid) scores

part2 =
  let l = (sort . getScores _Incomplete) scores
   in l !! (length l `div` 2)
