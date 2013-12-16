-- to use in a file:
-- :l <filename>
-- to reload:
-- :r

double_me x = x + x
double_us x y = x * 2 + y * 2

-- comments get double-minus-ed; 
-- this is basic style: use a simple function and biuld more complex ones
quad_us x y = double_me (double_me x) + double_me (double_me y)

-- haskell has an if then else!
triple_small x = if x > 100 then x else x * 3

-- and is not white space sensitive!
-- NB the else is mandatory. it NEEDS to return stuffs...
quint_smaller x = if x > 50
        then x
        else x * 5

-- 'definition': no args
-- NB ' can be used in func names; typically for non-lazy funcs
dan'derson = "Hello Daniel W"


-- bizarre: can't 'x = 4' in ghci; need to 'let x = 4' or in script.hs: 'x = 4'

-- lists are homogeneous; l = [1, 2, "hey"] wont work
alist = [1, 2, 3]

-- strings are lists...
helloworld = "hello" ++ " " ++ "world"

-- "{50millioncharacters}" ++ "a" is gonna be REAL slow: O(n) on first item;
-- NB: single char like C: 'a' != "a" (char vs 1-long list)
faster_cat = 'a' : "{50millioncharacters}"


-- comparisons are lexicographical
t = [3, 2, 1] > [2, 3] -- true, cause 3 > 2; doesn't matter about the rest


-- recursion (with lies about negative numbers....)
fact x = if x <= 0
        then 1
        else x * fact (x - 1)

-- list comprehensions:
blist = [x * 2 | x <- [1..10], x >= 5]

-- 'nested' list comps:
clist = [x ++ y | x <- ["a", "b", "c"], y <- ["d", "e", "f"]]


-- type declarations:
only_upper :: [Char] -> [Char]
only_upper st = [c | c <- st, c `elem` ['A'..'Z']]


-- the dull way...
add_vectors_a :: (Num a) => (a, a) -> (a, a) -> (a, a)
add_vectors_a a b = (fst a + fst b, snd a + snd b)
-- now with pattern matching!
add_vectors_b :: (Num a) => (a, a) -> (a, a) -> (a, a)
add_vectors_b (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- recursive pattern matching:
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- recursive maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list is just nonsense!"
maximum' [x] = x   -- well duh
maximum' (x:xs)         -- the max of the head vs all the rest:
    | x > max_xs = x    -- if head is greater, it's the head!
    | otherwise = max_xs  -- otherwise, it's the max of the rest
    where max_xs = maximum' xs  -- recursively defined...

maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "max on an emoty list is pure nonsense"
maximum2 [x] = x
maximum2 (x:xs) = max x (maximum2 xs)


--flippiing the input order of a function:
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x
