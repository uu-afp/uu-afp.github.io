---
title: Labs - Type-level programming
layout: margins
---

&nbsp;

## Type-level programming

Here is a list of exercises related to nested data types, GADTs,
and generic programming.

* [Contracts](#contracts)
* [Nested data types](#nested-data-types)
* [Generic parsing](#generic-parsing)

### Contracts

Here is a datatype of contracts:

```haskell
data Contract :: * -> * where
  Pred :: (a -> Bool) -> Contract a
  Fun  :: Contract a -> Contract b -> Contract (a -> b)
```

A contract can be a predicate for a value of arbitrary  type. For  functions, we
offer contracts that contain a precondition on the arguments, and a postcondition
on the results.

Contracts can be attached to values by means of `assert`. The idea is that
`assert` will cause run-time failure if a contract is violated, and otherwise
return the original result:

```haskell
assert :: Contract a -> a -> a
assert (Pred p)       x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre
```

For  function  contracts,  we first check the precondition on the value, then apply
the original function, and finally check the postcondition on the result. 
Note that the case for `Fun` makes use of the fact that the `Fun`
constructor targets only function contracts. Because of this knowledge, GHC
allows us to apply `f` as a function.

For example, the following contract states that a number is positive:

```haskell
pos :: (Num a, Ord a) => Contract a
pos = Pred (> 0)
```

We have

```haskell
assert pos 2 == 2
assert pos 0 == ⊥ (contract violation error)
```

**Exercise 1:** Define a contract

```haskell
true :: Contract a
```

such that for all values `x`, the equation

```haskell
assert true x == x
```

holds.  Prove this equation using equational reasoning.

Often, we want the postcondition of a function to be able to refer to the actual
argument that has been passed to the function. Therefore, let us change the type
of `Fun`:

```haskell
DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
```

The postcondition now depends on the function argument.

**Exercise 2:** Adapt the function `assert` to the new type of `DFun`.

**Exercise 3:** Define a combinator

```haskell
(==>) :: Contract a -> Contract b -> Contract (a -> b)
```

that reexpresses the behaviour of the old `Fun` constructor in terms of the new
and more general one.

**Exercise 4:** Define a contract suitable for the list index function
`(!!)`, i.e., a contract of type `Contract ([a] -> Int -> a)`
that checks if the integer is a valid index for the given list.

**Exercise 5:** Define a contract

```haskell
preserves :: Eq b => (a -> b) -> Contract (a -> a)
```

where `assert (preserves p) f x` fails if and only if the value of
`p x` is different from the value of `p (f x)`. Examples:

```haskell
assert (preserves length) reverse  "Hello"       == "olleH"
assert (preserves length) (take 5) "Hello"       == "Hello"
assert (preserves length) (take 5) "Hello world" == ⊥
```

**Exercise 6:** Consider

```haskell
preservesPos  = preserves (>0)
preservesPos' = pos ==> pos
```

Is there a difference between `assert preservesPos` and
`assert preservesPos'`? If yes, give an example where they show different
behaviour.  If not, try to prove their equality using equational reasoning.

We can add another contract constructor:

```haskell
List :: Contract a -> Contract [a]
```

The corresponding case of `assert` is as follows:

```haskell
assert (List c) xs = map (assert c) xs
```

**Exercise 7:** Consider

```haskell
allPos  = List pos
allPos' = Pred (all (> 0))
```

Describe the differences between `assert allPos` and `assert allPos'`,
and more generally between using `List` versus using `Pred`
to describe a predicate on lists. 
(Hint: Think carefully and consider different situations before giving your
answer. What about using the `allPos` and `allPos'` contracts as parts of
other contracts? What about lists of functions? What about infinite lists? 
What about strict and non-strict functions working on lists?)

### Nested data types

Here is a nested data type for square matrices:

```haskell
type Square      = Square' Nil  -- note that it is eta-reduced
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)

data Nil    a = Nil
data Cons t a = Cons a (t a)
```

**Exercise 1.** Give Haskell code that represents the following two square matrices as elements of the `Square` data type:

$$
\begin{pmatrix}
1 & 0 \\
0 & 1
\end{pmatrix}
\quad
\begin{pmatrix}
1 & 2 & 3 \\
4 & 5 & 6 \\
7 & 8 & 9
\end{pmatrix}
$$

Let's investigate how we can derive an equality function on square matrices. We do so very systematically by deriving an equality function for each of the four types. We follow a simple, yet powerful principle: type abstraction corresponds to term abstraction, and type application corresponds to term application.

What does this mean? If a type `f` is parameterized over an argument `a`, then in general, we have to know how equality is defined on `a` in order to define equality on `f a`. Therefore we define

```haskell
eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True
```

In this case, the `a` is not used in the definition of `Nil` , so it is not surprising that we do not use `eqA` in the definition of `eqNil`.  But what about `Cons`?  The data type `Cons` has two arguments `t` and `a`, so we expect two arguments to be passed to `eqCons`, something like

```haskell
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && ...
```

But what should the type of `eqT` be? The `t` is of kind `* -> *`, so it can't be `t -> t -> Bool`. We can argue that we should use `t a -> t a -> Bool`, because we use `t` applied to `a` in the definition of `Cons`. However, a better solution is to recognise that, being a type constructor of kind `* -> *`, an equality function on `t` should take an equality function on its argument as a parameter. And, moreover, it does not matter what this parameter is! A function like `eqNil` is polymorphic in type `a`, so let us require that `eqT` is polymorphic in the argument type as well:

```haskell
eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys
```

Now you can see how we apply `eqT` to `eqA` when we want equality at type `t a` -- the type application corresponds to term application.

**Exercise 2.** A type with a `forall` on the inside requires the extension `RankNTypes` to be enabled. Try to understand what the difference is between a function of the type of `eqCons` and a function with the same type but the `forall` omitted. Can you omit the `forall` in the case of `eqCons` and does the function still work?

Now, on to `Square'`. The type of `eqSquare'` follows exactly the same idea as the type of `eqCons`:

```haskell
eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
```

We now for the first time have more than one constructor, so we actually have to give multiple cases. Let us first consider comparing two applications of `Zero`:

```haskell
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
```

Note how again the structure of the definition follows the structure of the type.  We have a value of type `t (t a)`. We compare it using `eqT`, passing it an equality function for values of type `t a`. How? By using `eqT eqA`. The remaining cases are as follows:

```haskell
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA _         _         = False
```

The idea is the same -- let the structure of the recursive calls follow the structure of the type.

**Exercise 3.** Again, try removing the `forall` from the type of `eqSquare'`.  Does the function still
type check? Try to explain!

Now we're done:

```haskell
eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil
```

Test the function.  We can now also give an `Eq` instance for `Square` -- this requires the minor language  extension `TypeSynonymInstances`, because Haskell 98 does not allow type synonyms like `Square` to be used in  instance declarations:

```
instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)
```


**Exercise 4.** Systematically follow the scheme just presented in order to define a `Functor` instance for square matrices. I.e., derive a function `mapSquare` such that you can define

```haskell
instance Functor Square where
  fmap = mapSquare
```

This instance requires `Square` to be defined in eta-reduced form in the beginning, because Haskell does not allow partially applied type synonyms. If we had defined `Square` differently

```haskell
type Square a = Square' Nil a
```

we cannot make `Square` an instance of the class `Functor`.

**Exercise 5.** Why is this restriction in place? Try to find problems arising from partially applied type synonyms, and describe them (as concisely as possible) with a few examples.

<!-- ## Generic parsing -->

<!-- Haskell's `Show` and `Read` classes provide an easy way to display and -->
<!-- parse user-defined data structures. -->

<!-- Use GHC Generics and some parsing library (`uuparsinglib`, `attoparsec` -->
<!-- or `parsec`),  define a *generic* -->
<!-- `Parse` class. You may want to have a look at `Generic.Deriving.Show` -->
<!-- to see how a generic `Show` instance can be derived.  -->

<!-- Writing a generic read for all possible constructs is not feasible, -->
<!-- but try to cover as much of the language as you can. -->

<!-- * Start by handling only "basic" ADTs. To make it more precise, this means that it works for: -->

<!--     ```haskell -->
<!--     data Bool    = True | False -->
<!--     data IntTree = Leaf Int | Node IntTree IntTree -->
<!--     ``` -->

<!-- * Then take fixity of operators is taken into account. -->
<!--     To make it more precise, that means that the parser can handle -->
<!--     `Leaf 1 :|: (Leaf 2 :|: Leaf 3)` when `IntTree` is declared as: -->

<!--     ```haskell -->
<!--     data IntTree = Leaf Int | IntTree :|: IntTree -->
<!--     ``` -->

<!-- * Finally, support record labels. -->
<!--     To make it more precise, that means that the parser can handle -->
<!--     `Number { n = 1 }`  for a data type declared as: -->

<!--     ```haskell -->
<!--     data Number = Number { n :: Int } -->
<!--     ``` -->

<!-- What cases cannot be handled without backtracking? -->
