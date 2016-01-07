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

main = putStrLn $ show $ maximum $ primeFactors 600851475143 
