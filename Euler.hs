-- general routines of common use 
-- in solving Euler problems


module Euler 
( num2Digits
, digits2Num
) where

import Data.Char

num2Digits :: Integer -> [Int]
num2Digits n = map  digitToInt $ show n

digits2Num :: [Int] -> Integer
digits2Num a = read (map intToDigit a) :: Integer



 
