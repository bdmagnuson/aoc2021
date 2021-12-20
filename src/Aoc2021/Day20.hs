module Aoc2021.Day20
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.Vector (Vector)
import Data.Vector qualified as V

data Pixel = Light | Dark deriving (Eq)

$(makePrisms ''Pixel)

instance Show Pixel where
  show Light = "#"
  show Dark = "."

newtype Grid a = Grid {unGrid :: Vector (Vector a)}

instance (Show a) => Show (Grid a) where
  show = unlines . map (concatMap show) . V.toList . V.map V.toList . unGrid

(alg, img) = getInput "input/day20_test.txt" parser

parser = do
  alg <- many (pLight <|> pDark) <* P.endOfLine
  P.endOfLine
  img <- (Grid . V.fromList) <$> many (V.fromList <$> many (pDark <|> pLight) <* P.endOfLine)
  return (alg, img)
  where
    pLight = P.char '#' *> pure Light
    pDark = P.char '.' *> pure Dark

expand :: Pixel -> Grid Pixel -> Grid Pixel
expand p (Grid g) =
  let pad_v = V.fromList [p, p]
      g' = V.map (\x -> V.concat [pad_v, x, pad_v]) g
      pad_h = V.fromList (replicate 2 (V.generate (V.length (V.head g')) (const p)))
   in Grid (V.concat [pad_h, g', pad_h])

patch p = let l = [(subtract 1), id, (+ 1)] in [bimap a b p | a <- l, b <- l]

pass d (Grid g) = Grid (g & itraversed <.> itraversed %@~ f)
  where
    f (y, x) p = alg ^?! ix (indexG (Grid g) (y, x))
    h = V.length g
    w = V.length (V.head g)

    indexG (Grid g) (y, x) = toIndex (map f (patch (y, x)))
      where
        f (y, x) =
          case g ^? ix y . ix x of
            Nothing -> d
            Just a -> a

    toIndex = foldl go 0
      where
        go n k = n * 2 + (if k == Dark then 0 else 1)

enhance d = (pass d) . (expand d)

count = lengthOf (folded . folded . filtered (has _Light)) . unGrid

px1 = Dark

px2 = head alg

part1 = count (enhance px2 (enhance px1 img))

part2 = count ((iterate (enhance px2 . enhance px1)) img !! 25)
