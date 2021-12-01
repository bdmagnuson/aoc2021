module AocUtils
 ( getInput
 ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text.IO as T
import System.IO.Unsafe

getInput :: String -> P.Parser a -> a
getInput f p =
  case P.parseOnly p (unsafePerformIO (T.readFile f)) of
    Left e -> error e
    Right r -> r
