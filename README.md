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
