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
