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

data Fold = FoldX Int | FoldY Int deriving (Show)

data Point = Dot | Blank

instance Show Point where
  show Dot = "#"
  show Blank = "."

$(makePrisms ''Point)

instance Semigroup Point where
  Dot <> _ = Dot
  _ <> Dot = Dot
  Blank <> Blank = Blank

instance Monoid Point where
  mappend = (<>)
  mempty = Blank

newtype Paper a = Paper {unPaper :: [[a]]}

instance (Show a) => Show (Paper a) where
  show p =
    let a = (fmap . fmap) show (unPaper p)
     in unlines (map concat a)

instance (Semigroup a) => Semigroup (Paper a) where
  (<>) a b = let f = zipWith (<>) in Paper $ zipWith f (unPaper a) (unPaper b)

instance (Monoid a) => Monoid (Paper a) where
  mappend = (<>)
  mempty = Paper []

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

fold (FoldY y) (Paper p) =
  let (t, b) = splitAt y p
   in Paper t <> Paper (reverse (tail b))
fold (FoldX x) (Paper p) =
  let (l, r) = splitAt x (transpose p)
   in Paper (transpose l) <> Paper (transpose (reverse (tail r)))

(dots, folds) = getInput "input/day13.txt" parser

paper =
  let (mx, my) = (maximum (map fst dots) + 1, maximum (map snd dots) + 1)
      blank = take my (repeat (take mx (repeat Blank)))
      mark (x, y) p = p & ix y . ix x .~ Dot
   in Paper (foldr mark blank dots)

countDots :: Paper Point -> Int
countDots = lengthOf (folded . folded . filtered (has _Dot)) . unPaper

part1 = countDots (fold (head folds) paper)

part2 = foldl' (flip fold) paper folds
