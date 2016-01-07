-- general routines of common use 
-- in solving Euler problems


module Euler 
( num2Digits
, digits2Num
, fibs
) where

import Data.Char

num2Digits :: Integer -> [Int]
num2Digits n = map  digitToInt $ show n

digits2Num :: [Int] -> Integer
digits2Num a = read (map intToDigit a) :: Integer

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- my own prime number generator
-- more to write code than for efficiency

maxFactor :: Integer -> Integer
maxFactor n = fst $ last $ zip [1..] $ takeWhile (<= n) [f * f | f <- [1..]]

isPrime :: Integer -> Bool
isPrime n = and $ [ rem n f /= 0 | f <- [3,5 .. (maxFactor n)]]

coprimeTo30 = [1, 7, 11, 13, 17, 19, 23, 29]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] ++ [ n + r | n <- [30, 60 ..], r <- coprimeTo30, isPrime (n+r)]  

factors nn = [x | x <- [2.. maxFactor nn], rem nn x == 0] 
