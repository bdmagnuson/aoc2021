module Aoc2021.Day03
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import qualified Data.Attoparsec.Text as P
import Data.Char (ord)

input = getInput "input/day03.txt" parser

parser = many ((many (fmap toDigit P.digit)) <* P.endOfLine)
  where
    toDigit c = fromIntegral (ord c - 48)

boolToInt :: [Bool] -> Integer
boolToInt = foldl step 0
  where
    step a c = a * 2 + (if c then 1 else 0)

boolListToInt x = boolToInt (map (\x -> if x == 1 then True else False) x)

part1 = boolToInt gt * boolToInt (map not gt)
  where
    ones = foldl1 (zipWith (+)) input
    zeros = map (length input -) ones
    gt = zipWith (>) ones zeros

part2 = oxygen * co2
  where
    oxygen = boolListToInt $ part2_help (>=) input
    co2 = boolListToInt $ part2_help (<) input
    part2_help :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
    part2_help op x = go 0 x
      where
        go _ (x : []) = x
        go n xs = go (n + 1) $ filter (\x -> x !! n == p) xs
          where
            p =
              let ones = sum (map (!! n) xs)
                  zeros = length xs - ones
               in if ones `op` zeros then 1 else 0
