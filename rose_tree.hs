------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------
data Rose a = a :> [Rose a] deriving Show

tree = 1 :> map (\c -> c :> []) [1..5]

-- ===================================
-- Ex. 0-2
-- ===================================
root :: Rose a -> a
root (r :> _) = r

children :: Rose a -> [Rose a]
children (_ :> []) = []
children (_ :> xs) = xs

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . (!! 2) $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================
size :: Rose a -> Int
size (_ :> []) = 1
size (_ :> xs) = 1 + (sum . map size $ xs)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> xs) = sum . map leaves $ xs

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . (!! 2) . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================
instance Functor Rose where
  fmap f (x :> xs) = f x :> fmap (fmap f) xs

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (sin . fromIntegral) xs

-- ===================================
-- Ex. 11-13
-- ===================================
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a deriving (Show)
newtype Product a = Product a deriving (Show)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend x y = Sum (unSum x + unSum y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend x y = Product (unProduct x * unProduct y)

unSum :: Sum a -> a
unSum (Sum a) = a
unProduct :: Product a -> a
unProduct (Product a) = a

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================
class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap f xs = fold $ fmap f xs

instance Foldable [] where
    fold = foldr mappend mempty

instance Foldable Rose where
  fold (x :> []) = mappend x mempty
  fold (x :> xs) = mappend x (fold (fmap fold xs))

t = 1 :> [2 :> [], 3 :> [4 :> []]]
t' = fmap Product t
ex14 = unProduct $ fold t'

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . (!! 2) . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================
t16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
ex16 = unSum $ foldMap Sum t16

ex17 = unSum (mappend (mappend (foldMap Sum xs) (mappend (foldMap Sum . (!! 2) . children $ xs) (Sum 30))) (foldMap Sum . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap Sum xs) (Sum (unProduct (mappend (foldMap Product . (!! 2) . children $ xs) (Product 3))))) (foldMap Sum . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================
fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum xs = unSum $ foldMap Sum xs
fproduct xs = unProduct $ foldMap Product xs

ex21 = ((fsum . (!! 1) . children $ xs) + (fproduct . head . children . head . children . (!! 2) . children $ xs)) - (fsum . head . children . head . children $ xs)


main = putStrLn "Lab 6"
