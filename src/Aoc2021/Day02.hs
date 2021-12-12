module Aoc2021.Day02
  ( part1,
    part2,
    bench,
  )
where

import Aoc2021.AocUtils
import Control.Applicative
import Data.Attoparsec.Text qualified as P

data Dir
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

data Loc = Loc
  { horz :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Show)

input = getInput "input/day02.txt" parser

parser = many ((pForward <|> pDown <|> pUp) <* P.endOfLine)
  where
    pForward = Forward <$> (P.string "forward " *> P.decimal)
    pDown = Down <$> (P.string "down " *> P.decimal)
    pUp = Up <$> (P.string "up " *> P.decimal)

move1 l x =
  case x of
    Forward d -> l {horz = horz l + d}
    Down d -> l {depth = depth l + d}
    Up d -> l {depth = depth l - d}

move2 l x =
  case x of
    Forward d ->
      l
        { horz = horz l + d,
          depth = depth l + (aim l * d)
        }
    Down d -> l {aim = aim l + d}
    Up d -> l {aim = aim l - d}

part1 x = let Loc h z _ = foldl move1 (Loc 0 0 0) x in h * z

part2 x = let Loc h z _ = foldl move2 (Loc 0 0 0) x in h * z

bench = do
  input <- getInput' "input/day02.txt" parser
  return $ seq (part1 input, part2 input) ()
