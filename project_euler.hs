import Data.Char

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

-- problem 6: sum squares
--
getSumSquares :: Integer -> Integer
getSumSquares n = sum $ map (^2) [1 .. n]

getSquareSums :: Integer -> Integer
getSquareSums n = (^2) $ sum [1 .. n]

euler6 :: Integer -> Integer
euler6 n = (-) (getSquareSums n) (getSumSquares n)

-- problem 7: primes
-- this is of course naive and slow, but shows the filtering concept
getPrimesUpTo :: Integer -> [Integer]
getPrimesUpTo a = filter (\i -> (length $ findFactors i) == 1) [2,3..a]

isDivisibleBy :: [Integer] -> Integer -> Bool
isDivisibleBy factors number = any (isFactor number) factors

getPrimesUpTo' :: Integer -> [Integer]
getPrimesUpTo' n
    | n <= 1 = []
    | n == 2 = [2]
    | isDivisibleBy primes n = primes
    | otherwise = primes ++ [n]
    where primes = getPrimesUpTo' (n-1)


-- problem 8: consecutive integer product
--slices
--
slice :: Int -> Int -> [Char] -> [Char]
slice from to xs = take (to - from + 1 ) (drop from xs)

getProd :: Int -> Int -> [Char] -> Int
getProd from to xs = foldl (*) 1 $ map (\x -> ord(x) - ord('0')) ys
    where ys = slice from to xs

euler8Slice :: Int -> [Char] -> (Int, [Char])
euler8Slice start xs = (getProd start (start + 13 - 1) xs, slice start (start + 13 - 1) xs)

euler8num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

--euler8 = maximum $ map (\x -> product $ take 13 $ drop x $ show euler8num) [0,1.. (length $ show euler8num) ]
--

--euler9 pythagorean triple sum = 1000
sumTuple :: (a, a, a) -> a
sumTuple (x, y, z) = x + y +z

sumx :: a -> [(b, c, d)]
sumx x = filter (\s -> sumTuple s == x) [ (i, j, k) | i <- [1 .. x], j <- [i+1..x], k <- [j+1..x]]

isPythag :: (Num a, Eq a) => (a, a, a) -> Bool
isPythag (x, y, z) = (x^2 + y^2) == (z^2)



