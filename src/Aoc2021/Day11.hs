module Aoc2021.Day11
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Char (ord)
import Data.List (findIndex)
import Data.Vector qualified as V

data Spot = Spot
  { _flash :: Bool,
    _entry :: Int
  }

$(makeLenses ''Spot)

type Grid = V.Vector (V.Vector Spot)

type Pt = (Int, Int)

input = getInput "input/day11.txt" parser

parser = V.fromList <$> many ((V.fromList . map ((Spot False) . subtract 48 . ord)) <$> many P.digit <* P.endOfLine)

neighbors (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x + 1, y + 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x - 1, y - 1)
  ]

allPts x = fmap fst $ x ^@.. itraversed <.> itraversed

resetFlash x = x & traversed . traversed . flash .~ False

step :: Grid -> Grid
step g = go (resetFlash g) (allPts g)
  where
    go g [] = g
    go g (p : ps) =
      let (g', newPts) = incr g p
       in go g' (ps ++ newPts)
    incr :: Grid -> Pt -> (Grid, [Pt])
    incr g pt@(y, x) =
      case g ^? ix y . ix x of
        Nothing -> (g, [])
        Just (Spot True _) -> (g, [])
        Just (Spot False 9) -> (g & ix y . ix x .~ Spot True 0, neighbors pt)
        Just (Spot False n) -> (g & ix y . ix x .~ Spot False (n + 1), [])

countFlashes = lengthOf (folded . folded . flash . filtered id)

part1 = sum . map countFlashes . take 101 $ (iterate step input)

part2 = findIndex allFlash (iterate step input)
  where
    allFlash x = and (x ^.. traversed . traversed . flash)
