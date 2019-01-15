--file: main.hs
--abuhay abune
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Data.Char
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import System.Random
import Numeric (showHex, showIntAtBase)
import ParserLearn


checkParseBin :: Int -> String -> Bool
checkParseBin n sx | n < 0     = True
                   | otherwise = y == (Just n) && str == ys
                                where (y,ys) = parseBin (num ++ str)
                                      num    = showIntAtBase 2 intToDigit n ""
                                      str    = filter isAlpha sx



main = do 
      quickCheck checkParseBin
