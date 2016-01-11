-- general routines of common use 
-- in solving Euler problems


module Euler 
( num2Digits
, digits2Num
, fibs
, maxFactor
, isPrime
, primeFactors
, factors
, primes


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