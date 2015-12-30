module Lab5 where
import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================
action :: Concurrent a -> Action
action c = c (\a -> Stop)

-- ===================================
-- Ex. 1
-- ===================================
stop :: Concurrent a
stop = \c -> Stop

-- ===================================
-- Ex. 2
-- ===================================
atom :: IO a -> Concurrent a
atom m = \c -> Atom (do a <- m; return (c a))

-- ===================================
-- Ex. 3
-- ===================================
fork :: Concurrent a -> Concurrent ()
fork m = \c -> Fork (action m) (c ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par m n = \c -> Fork (m c) (n c)

-- ===================================
-- Ex. 4
-- ===================================
instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)

-- ===================================
-- Ex. 5
-- ===================================
roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roudRobin (a:as) = case a of
                       Atom m -> do
                           b <- m
                           roundRobin (as ++ [b])
                       Fork m n -> roundRobin (as ++ [m, n])
                       Stop -> roundRobin as

-- ===================================
-- Tests
-- ===================================
ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")

-- ===================================
-- Helper Functions
-- ===================================
run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

-- Takes a list of integers, and for each integer will fork a process 
-- that will print the integer and terminate
loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

-- Writes a string to output, and appends itself to the end of the
-- concurrenct round robin
loopStr :: String -> Concurrent ()
loopStr s = do atom (putStrLn s); loopStr s

ex2 :: Concurrent ()
ex2 = do
        atom (putStrLn "Starting ex2!")
        fork (loopStr "First")
        loopStr "Second"
