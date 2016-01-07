-- If we list all the natural numbers below 10 that are multiples
-- of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000

euler001 n = sum [0, 3 ..n] + sum [0, 5 ..n] - sum [0, 15 ..n]

main = putStrLn $ show (euler001 1000)

