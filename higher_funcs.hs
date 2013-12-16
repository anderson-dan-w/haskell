--flippiing the input order of a function:
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x

-- what's the largest number, under 100k, divisible by 3829
biggest_100div_by3829 :: (Integral a) => a
biggest_100div_by3829 = head (filter div_by [100000, 99999..])
    where div_by x = x `mod` 3829 == 0

--more generally, any max:
biggest_div_by3829 :: (Integral a) => a -> a
biggest_div_by3829 x = head (filter div_by [x, x-1..])
    where div_by y = y `mod` 3829 == 0

--most generally:
biggest_div_by :: (Integral a) => a -> a -> a
biggest_div_by x y = head (filter div_by [x, x-1..])
    where div_by z = z `mod` y == 0

-- with lambdas:
biggest_w_lamb :: (Integral a) => a -> a -> a
biggest_w_lamb x y = head (filter (\z -> z `mod` y == 0) [x, x-1..])


--folding and currying: foldl takes a binary operator, a starting val and a list
-- so product_list provides it the operator and the starter; which will return
-- the curried function that now takes a list
product_list :: (Num a) => [a] -> a
product_list = foldl (*) 1

