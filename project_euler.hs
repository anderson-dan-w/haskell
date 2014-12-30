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

-- problem 3: prime factors of n
-- let's do it the dumb way, at least to start
isFactor :: (Integral a) => a -> a -> Bool
isFactor n i
    | n `mod` i == 0 = True
    | otherwise      = False

findFactors' :: (Integral a) => a -> a -> [a]
findFactors' n i
    | n <= 1    = [] -- ignore 0 and negatives for now
    | otherwise = [x] ++ (findFactors' (round ( toRational n / toRational x )) x)
       where x = head $ filter (\i -> isFactor n i) [i..n]

findFactors :: (Integral a) => a -> [a]
findFactors n = findFactors' n 2

-- problem 4: palindromic numbers
-- 
testPalindrome :: (Eq a) => [a] -> Bool
testPalindrome n
    | length n <= 1 = True
    | head n == last n && (testPalindrome $ tail $ init n) = True
    | otherwise        = False

-- this is slow-ish...
euler4 :: (Integral a, Show a) => a -> a
euler4 n = maximum $ filter (testPalindrome . show) [i*j | i <- [n,n-1..2], j <- [i, i-1..2]]
