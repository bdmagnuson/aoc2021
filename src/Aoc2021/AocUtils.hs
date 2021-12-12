module Aoc2021.AocUtils
  ( getInput,
    getInput',
    revsort,
  )
where

import Data.Attoparsec.Text qualified as P
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text.IO qualified as T
import System.IO.Unsafe

getInput :: String -> P.Parser a -> a
getInput f p =
  case P.parseOnly p (unsafePerformIO (T.readFile f)) of
    Left e -> error e
    Right r -> r

getInput' :: String -> P.Parser a -> IO a
getInput' f p =
  T.readFile f >>= \x -> case P.parseOnly p x of
    Left e -> error e
    Right r -> return r

revsort :: Ord a => [a] -> [a]
revsort = sortBy (comparing Down)
