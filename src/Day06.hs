module Day06
  ( part1,
    part2,
  )
where

import AocUtils
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.Text as P
import qualified Data.IntMap as M

input = getInput "input/day06.txt" parser

parser = P.decimal `P.sepBy` (P.char ',')

initFish :: [Int]
initFish = map (f $ foldl inc M.empty input) [0 .. 8]
  where
    inc m p = m & at p . non 0 %~ succ
    f m i = case M.lookup i m of
      Nothing -> 0
      Just n -> n

solve x = sum $ (head . drop x) (iterate advance initFish)
  where
    advance xs = (tail xs ++ [head xs]) & ix 6 +~ head xs

part1 = solve 80

part2 = solve 256
