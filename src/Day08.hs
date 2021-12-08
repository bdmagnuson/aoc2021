{-# LANGUAGE TemplateHaskell #-}

module Day08
  ( part1,
    part2,
  )
where

import AocUtils
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.Text as P
import Data.Char
import Data.List (find, permutations, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

data Display = Display
  { _patterns :: [Text],
    _display :: [Text]
  }
  deriving (Show)

$(makeLenses ''Display)

input = getInput "input/day08.txt" parser

parser = many (pLine <* P.endOfLine)
  where
    pLine = do
      patterns <- P.takeWhile1 isAlpha `P.sepBy` P.char ' '
      P.string " | "
      display <- P.takeWhile1 isAlpha `P.sepBy` P.char ' '
      return (Display patterns display)

part1' = lengthOf (folded . display . folded . filtered (\x -> let l = T.length x in or [l == 2, l == 4, l == 3, l == 07])) input

validNumbers =
  M.fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

translate :: M.Map Char Char -> Text -> Text
translate m = T.pack . sort . T.unpack . T.map (\x -> m ^?! ix x)

findPermutation :: [Text] -> M.Map Char Char
findPermutation l = fromJust $ find (isValid l) (map (\x -> M.fromList $ T.zip "abcdefg" x) perms)
  where
    perms = map T.pack (permutations (T.unpack "abcdefg"))
    isValid l p = all (\x -> translate p x `M.member` validNumbers) l

decode :: Display -> Int
decode (Display p d) = toNum (map (translate (findPermutation p)) d)
  where
    toNum = foldl step 0
      where
        step a c = a * 10 + (validNumbers ^?! ix c)

part2 = sum (map decode input)
