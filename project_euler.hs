-- problem 1: sum of all mults of 3 and 5 below 1000 (m1, m2 below top)
sum_mult_below :: (Integral a) => a -> a -> a -> a
sum_mult_below top m1 m2 = sum $ filter (\x -> x `mod` m1 == 0 || x `mod` m2 == 0) [1,2..top-1]

-- problem 2: sum even fibs < 4000000
-- fib i dont get yet: - infinite list!
fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

s = sum $ takeWhile (<4000000) $ filter even fibs

-- unnecessary, but learning types...
-- I give it an int (the 2**63rd fibonacci number is high enough)...
--   and it gives me an Integer in return because fibs = [Integer], duh
fib ::  Int -> Integer
fib x = fibs !! x


