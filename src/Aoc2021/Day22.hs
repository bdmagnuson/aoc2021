module Aoc2021.Day22
  ( part1,
    part2,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.List (foldl', tails)

data Flip = On | Off deriving (Show)

type Range = (Int, Int)

data Cuboid = Cuboid Range Range Range deriving (Show, Eq)

type Solid = [Cuboid]

data Direction = Direction Flip Cuboid deriving (Show)

input = getInput "input/day22.txt" parser

parser = many $ do
  dir <- pFlip
  rng <- pCuboid `P.sepBy1` P.char ',' >>= \[x, y, z] -> return $ Cuboid x y z
  P.endOfLine
  return $ Direction dir rng
  where
    pCuboid = do
      P.anyChar
      P.anyChar
      from <- pNumber
      P.string ".."
      to <- pNumber
      return (from, to)
    pNumber = P.signed P.decimal
    pFlip = (P.string "on " *> pure On) <|> (P.string "off " *> pure Off)

differenceS :: Solid -> Solid -> Solid
differenceS s1 s2 = foldl' (\s c -> concatMap ((flip differenceC) c) s) s1 s2

unionS :: Solid -> Solid -> Solid
unionS s1 s2 = differenceS s1 s2 ++ s2

differenceC :: Cuboid -> Cuboid -> Solid
differenceC (c@(Cuboid (ax1, ax2) (ay1, ay2) (az1, az2))) (Cuboid (bx1, bx2) (by1, by2) (bz1, bz2)) =
  let dx = difference1D (ax1, ax2) (bx1, bx2)
      dy = difference1D (ay1, ay2) (by1, by2)
      dz = difference1D (az1, az2) (bz1, bz2)

      extendY :: ([Range], Range) -> ([Range], Range) -> ([(Range, Range)], (Range, Range))
      extendY (nxs, ox) (nys, oy) =
        let s1 = [((x1, x2), (ay1, ay2)) | (x1, x2) <- nxs]
            s2 = let (x1, x2) = ox in [((x1, x2), (y1, y2)) | (y1, y2) <- nys]
         in (s1 ++ s2, (ox, oy))

      extendZ :: ([(Range, Range)], (Range, Range)) -> ([Range], Range) -> ([(Range, Range, Range)], (Range, Range, Range))
      extendZ (nxys, oxy) (nzs, oz) =
        let s1 = [((x1, x2), (y1, y2), (az1, az2)) | ((x1, x2), (y1, y2)) <- nxys]
            s2 = let ((x1, x2), (y1, y2)) = oxy in [((x1, x2), (y1, y2), (z1, z2)) | (z1, z2) <- nzs]
         in (s1 ++ s2, (\(p1, p2) p3 -> (p1, p2, p3)) oxy oz)
   in case (dx, dy, dz) of
        (Nothing, _, _) -> [c]
        (_, Nothing, _) -> [c]
        (_, _, Nothing) -> [c]
        (Just (nxs, ox), Just (nys, oy), Just (nzs, oz)) ->
          let (nxys, oxy) = extendY (nxs, ox) (nys, oy)
              (nxyzs, _) = extendZ (nxys, oxy) (nzs, oz)
           in [Cuboid x y z | (x, y, z) <- nxyzs]

difference1D :: Range -> Range -> Maybe ([Range], Range)
difference1D (x1, x2) (y1, y2) =
  if (y1 > x2) || (y2 < x1)
    then Nothing
    else case (compare x1 y1, compare x2 y2) of
      (EQ, EQ) -> type1
      (EQ, LT) -> type1
      (EQ, GT) -> type2
      (GT, EQ) -> type1
      (GT, LT) -> type1
      (GT, GT) -> type2
      (LT, EQ) -> type3
      (LT, LT) -> type3
      (LT, GT) -> type4
  where
    type1 = Just ([], (x1, x2))
    type2 = Just ([(y2 + 1, x2)], (x1, y2))
    type3 = Just ([(x1, y1 - 1)], (y1, x2))
    type4 = Just ([(x1, y1 - 1), (y2 + 1, x2)], (y1, y2))

intersectionC :: Cuboid -> Cuboid -> Maybe Cuboid
intersectionC (Cuboid ax ay az) (Cuboid bx by bz) =
  case (difference1D ax bx, difference1D ay by, difference1D az bz) of
    (Just (_, ox), Just (_, oy), Just (_, oz)) -> Just (Cuboid ox oy oz)
    otherwise -> Nothing

testOverlaps :: Solid -> Bool
testOverlaps s = and [all (\x -> (intersectionC c x) == Nothing) cs | (c : cs) <- tails s]

unionC :: Cuboid -> Cuboid -> Solid
unionC x y =
  case intersectionC x y of
    Nothing -> [x, y]
    Just i -> y : (differenceC x i)

solve x = foldl' f [] x
  where
    f s1 (Direction Off s2) = differenceS s1 [s2]
    f s1 (Direction On s2) = unionS s1 [s2]

area = sum . map f
  where
    f (Cuboid p1 p2 p3) = dim p1 * dim p2 * dim p3
    dim (a, b) = abs (a - b) + 1

trim = map clamp . filter f
  where
    f (Direction _ (Cuboid p1 p2 p3)) = all (\(a, b) -> b >= (-50) && a <= 50) [p1, p2, p3]
    clamp (Direction d (Cuboid p1 p2 p3)) =
      let p1' = go p1
          p2' = go p2
          p3' = go p3
          go (a, b) = (max (-50) a, min 50 b)
       in Direction d (Cuboid p1' p2' p3')

part1 = (area . solve . trim) input

part2 = (area . solve) input
