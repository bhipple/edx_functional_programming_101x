-- Implementation for concat
lstconcat :: [[a]] -> [a]
lstconcat [] = []
lstconcat (xs : xss) = xs ++ lstconcat xss

-- Implementation for replicate
rep :: Int -> a -> [a]
rep 0 _ = []
rep n x = x : rep (n-1) x

-- Implementation for !!
nth :: [a] -> Int -> a
nth (x : _) 0 = x
nth (_ : xs) n = nth xs (n - 1)

-- Merge two sorted lists
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Mergesort implemenntation
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs
