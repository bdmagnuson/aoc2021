module Aoc2021.Day09
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Set qualified as S hiding (foldr)
import Data.Vector qualified as V

input = getInput "input/day09.txt" parser

parser = V.fromList <$> many ((V.fromList . map (subtract 48 . ord)) <$> many P.digit <* P.endOfLine)

height (x, y) = input ^? ix y . ix x

neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lowPts = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1], isLow (x, y)]
  where
    h = V.length input
    w = V.length (input V.! 0)
    isLow p = all (\x -> pure x > height p) (mapMaybe height . neighbors $ p)

basin p =
  case height p of
    Nothing -> S.empty
    Just 9 -> S.empty
    Just h -> foldr S.union (S.singleton p) (map basin higherPts)
      where
        higherPts = filter (\x -> height x > Just h) (neighbors p)

part1 = sum . map (+ 1) . mapMaybe height $ lowPts

part2 = product . take 3 . revsort . map (S.size . basin) $ lowPts
