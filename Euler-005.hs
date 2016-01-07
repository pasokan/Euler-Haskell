-- 2520 is the smallest number that can be divided by each of 
-- the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is
-- evenly divisible by all of the numbers from 1 to 20?

-- **************************
-- This is trivial with foldl
-- and the built-in lcm
-- **************************

main = do 
    putStrLn $ show $ foldl lcm 2520 [11..20]
    putStrLn $ show $ foldl lcm 1 [1..20]
