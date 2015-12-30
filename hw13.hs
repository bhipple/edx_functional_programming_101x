import Test.QuickCheck

-- Finding correct definitions of foldl in terms of foldr
foldlA f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a
foldlB f a bs = foldr (\a b -> f b a) a bs
foldlC f = flip $ foldr (\a b g -> b (f g a)) id
foldlD = foldr . flip

prop_a :: String -> [String] -> Bool
prop_a x xs = foldl (++) x xs == foldlA (++) x xs

prop_b :: String -> [String] -> Bool
prop_b x xs = foldl (++) x xs == foldlB (++) x xs

prop_c :: String -> [String] -> Bool
prop_c x xs = foldl (++) x xs == foldlC (++) x xs

prop_d :: String -> [String] -> Bool
prop_d x xs = foldl (++) x xs == foldlD (++) x xs

checks = [prop_a, prop_b, prop_c, prop_d]

main = do
    mapM_ quickCheck checks
    putStrLn "Ran checks"
