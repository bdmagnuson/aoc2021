module Aoc2021.Day13
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text qualified as P
import Data.List (foldl', splitAt, transpose, zipWith)
import Data.Semigroup

data Point = Dot | Blank

instance Show Point where
  show Dot = "#"
  show Blank = "."

$(makePrisms ''Point)

instance Semigroup Point where
  Dot <> _ = Dot
  _ <> Dot = Dot
  Blank <> Blank = Blank

newtype Paper a = Paper {unPaper :: [[a]]}

instance (Show a) => Show (Paper a) where
  show = unlines . map concat . fmap (fmap show) . unPaper

instance (Semigroup a) => Semigroup (Paper a) where
  (<>) a b = let f = zipWith (<>) in Paper $ zipWith f (unPaper a) (unPaper b)

data Fold = FoldX Int | FoldY Int deriving (Show)

parser = do
  dots <- many (pCoord <* P.endOfLine)
  P.endOfLine
  folds <- many (pFold <* P.endOfLine)
  return (dots, folds)
  where
    pCoord = do
      x <- P.decimal
      P.char ','
      y <- P.decimal
      return (x, y)
    pFold =
      P.string "fold along "
        *> ( (FoldY <$> (P.string "y=" *> P.decimal))
               <|> (FoldX <$> (P.string "x=" *> P.decimal))
           )

(dots, folds) = getInput "input/day13.txt" parser

paper =
  let (mx, my) = (maximum (map fst dots) + 1, maximum (map snd dots) + 1)
      blank = take my (repeat (take mx (repeat Blank)))
      mark (x, y) = ix y . ix x .~ Dot
   in Paper (foldr mark blank dots)

fold (Paper p) (FoldY y) =
  let (t, b) = splitAt y p
   in Paper t <> Paper (reverse (tail b))
fold (Paper p) (FoldX x) =
  let (l, r) = splitAt x (transpose p)
   in Paper (transpose l) <> Paper (transpose (reverse (tail r)))

countDots :: Paper Point -> Int
countDots = lengthOf (folded . folded . filtered (has _Dot)) . unPaper

part1 = countDots (fold paper (head folds))

part2 = foldl' fold paper folds
