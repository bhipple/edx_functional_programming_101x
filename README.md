## Functional Programming 101x
Course notes for [https://www.edx.org/course/introduction-functional-programming-delftx-fp101x-0](Introduction to Functional Programming with Erik Meijer on edx.org)

See [https://github.com/fptudelft/FP101x-Content-2015](the course repository on github.com) for more materials.

### Week 1
When in GHCi, use `:?` to see all commands that can be run in GHCi.

### Week 2: Types
If evaluating an expression `e` would produce a value of type `t`, then `e` has type `t`, written `e :: t`

Every well formed expression has a type, automatically calculated by type inference if not specified explicitly.

Lists are homogenous.  Tuples are heterogenous, but the type of the tuple is based on the elements in each index: for example, `('a', (False, 'b')) :: ((Char,(Bool,Char))`

Functions with multiple arguments are always curried!  Note that unlike most other operators, the `->` in Haskell associates to the right. This lets us skip many parenthesis when writing function signatures:

`Int -> Int -> Int -> Int` is equivalent to `Int -> (Int -> (Int -> Int))`

With function application, arguments associate to the left:

`mult x y z` is equivalent to `(((mult x) y) z)`

#### Overloading Functions
Polymorphic functions are said to be overloaded if their type has one or more constraints.

This function takes a list of any type `[a]` provided that the type `a` is of type `Num`
`sum :: Num a => [a] -> a`

### Week 3: List Comprehensions and Recursive Functions
List comprehensions are often the most convenient way to express things:

`[x^2 | x <- [1..5]]` generates `[1,4,9,16,25]`
`[(x,y) | x <- [1,2,3], y <- [4,5]]` generates `[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]`
If we swap the order of the generators on the RHS, the for loops generating the pairs are reversed:
`[(x,y) | y <- [4,5], x <- [1,2,3]]` generates `[(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]`

We can also have dependent generators:
`[(x,y) | x <- [1..3], y <- [x..3]]` generates `[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]`

We can also add `Guards` or `Filters` after the generators:
`[x | x <- [1..10], even x]`
```
factors :: Int [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
```
Zip is sweet, 'nuff said.

Creating a function that returns the list of all pairs of adjacent elements in a list:
```
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
```

### Week 4: Higher Order Functions
A function is called higher-order if it takes a function as an argument or returns a function as a result. For example,
```
twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> a) -> [a] -> [a]
map f [] = []
map f (x:xs) = f x : map f xs
```
We can also define these things using list comprehensions:  
`map f xs = [f x | x <- xs]`

These are all instances of a common pattern `foldr`
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

sum :: [a] -> a
sum xs = foldr (+) 0 xs

product :: [a] -> a
product xs = foldr (*) 1 xs

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

or :: [Bool] -> Bool
or xs = foldr (||) False xs

reverse :: [a] -> [a]
reverse = foldr (\ x xs -> xs ++ [x]) []
```
Foldr is a homomorphism over a list.

Another fundamental higher order function is the composition operator (.)
```
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```
An example is:
```
odd :: Int -> Bool
odd = not . even
```

### Week 5: Parsers and Parser Combinators
For simplicity, we won't use the Maybe type in this lecture; the parser will either fail and return the empty list of results, or succeed.  
`type Parser a :: String -> [(a,String)]`

A simple parser `item` fails if the input is empty; otherwise, it consumes the first char
```
item :: Parser Char
item = \inp -> case inp of
                []      -> []
                (x:xs)  -> [(x,xs)]
```
A failure parser:
```
failure :: Parser a
failure = \inp -> []
```
### Week 6: Interactive Programs
IO type functions specify actions, whereas pure functions are expressions.

In this week's lectures, we implement a Hangman game to demonstrate taking user input and printint output in an interactive program.

### Week 7: Types and Classes
We can define aliases for existing types using the `type` keyword.  These are simple type declarations:
```
type String = [Char]
type Pair a = (a,a)
type Pos = (Int,Int)
```
Data declarations specify a completely new type:
```
data Bool = False | True
```
These are often referred to as `Algebraic Data Types`.  The two values False and True are called the `constructors` for the type `Bool`.

We can look at these as context free grammars.
```
-- Declares a new algebraic data type
data Answer = Yes | No | Unknown

-- Creates a list of answers
answers :: [Answer]
answers = [Yes, No, Unknown]

-- Function operating on answers
flip :: Answer -> Answer
flip Yes = No
flip No = yes
flip Unknown = Unknown
```

#### Recursive Types
Consider the church numerals:  
`data Nat = Zero | Succ Nat`. It has a constructor `Zero :: Nat` and `Succ :: Nat -> Nat`

With a helper function, we can define the isomorphism between Nat data and natural numbers:
```
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
```

Playing with this further, we can define:
```
add Zero n = n
add (Succ m) n = Succ (add m n)
```

#### Arithmetic Expressions
```
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

-- Count how many values appear in an expression
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

-- Evaluate an expression
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- We can also replace the above expression with a fold
eval = fold id (+) (*)
```
#### Binary Trees
```
data Tree = Leaf Int
          | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs m (Left n) = m == n
occurs m (Node l n r) = m == n
                        || occurs m l
                        || occurs m r

-- Convert a tree into an list from an in-order traversal
flatten :: Tree -> [Int]
flatten (Left n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r
```

### Week 10: The Countdown Problem
Given a set of integers, a target number, and the arithmetic operators `+, -, *, /`, construct an expression whose value is the target number.

See `countdown.hs` for exercises on the countdown problem.

### Week 11: Lazy Evaluation
Consider a fucntion `square n = n * n` and an input `square (3 + 4)`.

We can expand to either one of these results with identical results:
```
square (3+4)
square (7)
7 * 7
49

square (3+4)
(3+4) * (3+4)
7 * 7
49
```
The first is called applicative order reduction, while the second is called normal order reduction. Normal order results in excess computation!

Consider expressions like `fst (1, last [1..])`.  Normal order works instantly, while applicative order fails to terminate.

Take away points:  
* Normal order reduction may give a result when applicative order fails to terminate
* For any given expression, if there exists any reduction sequence that terminates, then normal order reduction also terminates with the same result.
* Normal order reduction is more likely to terminate
* Applicative order reduction may be more performant, when it terminates

How Haskell handles this dilemma:  
Expressions are passed as pointers to thunks. When the thunk is evaluated and someone queries the value a second time, no computation is done.  
This allows Haskell to do normal order evaluation without the performance hit!  In the above example, the two `(3+4)` computations are both pointing to the same thunk, so the addition only happens once.

#### Generating Primes
See https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
