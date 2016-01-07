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

isPrime :: Integer -> Bool
isPrime n = if rem n 2 == 0 then False else and $ [ rem n f /= 0 | f <- [3,5 .. (maxFactor n)]]

factors :: Integer -> [Integer]
factors nn = concat  $ [[x, div nn x] | x <- [2.. maxFactor nn], rem nn x == 0]

primeFactors :: Integer -> [Integer]
primeFactors nn = [ f | f <- factors nn, isPrime f]

primes = primesBelow30 ++ [ n + r | n <- [30, 60 ..], r <- coprimeTo30, isPrime (n+r)]  
    where primesBelow30 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
          coprimeTo30 = [1, 7, 11, 13, 17, 19, 23, 29]
