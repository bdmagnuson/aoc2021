module Day09
  ( part1,
    part2,
  )
where

import AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Set qualified as S hiding (foldr)
import Data.Vector qualified as V

input = getInput "input/day09.txt" parser

parser = V.fromList <$> many ((V.fromList . map (subtract 48 . ord)) <$> many P.digit <* P.endOfLine)

height (x, y) = input ^? ix y . ix x

lowPts = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1], isLow (x, y)]
  where
    h = V.length input
    w = V.length (input V.! 0)
    isLow (x, y) = all (pt <) neighbors
      where
        pt = input ^?! ix y . ix x
        neighbors = (catMaybes . map height) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

basin (x, y) =
  case height (x, y) of
    Nothing -> S.empty
    Just 9 -> S.empty
    Just h -> foldr S.union (S.singleton (x, y)) (map basin higherPts)
      where
        higherPts = filter (\x -> height x > Just h) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

part1 = sum . map (+ 1) . catMaybes . map height $ lowPts

part2 = product . take 3 . reverse . sort . map (S.size . basin) $ lowPts
