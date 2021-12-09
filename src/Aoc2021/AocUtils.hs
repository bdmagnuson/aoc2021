module Aoc2021.AocUtils
  ( getInput,
  )
where

import Data.Attoparsec.Text qualified as P
import Data.Text.IO qualified as T
import System.IO.Unsafe

getInput :: String -> P.Parser a -> a
getInput f p =
  case P.parseOnly p (unsafePerformIO (T.readFile f)) of
    Left e -> error e
    Right r -> r
