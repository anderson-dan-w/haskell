quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ 
                    [x] ++ 
                    quicksort [z | z <- xs, z > x]

quicksort_f :: (Ord a) => [a] -> [a]
quicksort_f [] = []
quicksort_f (x:xs) = quicksort_f (filter (<=x) xs) ++ 
                        [x] ++
                        quicksort_f (filter (>x) xs)

quicksort_f2 :: (Ord a) => [a] -> [a]
quicksort_f2 [] = []
quicksort_f2 (x:xs) = 
    let lesser = quicksort_f2 (filter (<=x) xs)
        greater = quicksort_f2 (filter (>x) xs)
    in lesser ++ [x] ++ greater
