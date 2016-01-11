-- my own prime number generator
-- more to write code than for efficiency

module Primes
( isPrime
, primes
) where

maxFactor :: Integer -> Integer
maxFactor n = fst $ last $ zip [1..] $ takeWhile (<= n) [f * f | f <- [1..]]

isPrime :: Integer -> Bool
isPrime n
  | n < 30       = n `elem` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  | rem n 2 == 0 = False
  | otherwise    = and $ [ rem n f /= 0 | f <- [3,5 .. (maxFactor n)]]

primes = primesBelow30 ++ [ n + r | n <- [30, 60 ..], r <- coprimeTo30, isPrime (n+r)]
    where primesBelow30 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
          coprimeTo30 = [1, 7, 11, 13, 17, 19, 23, 29]
