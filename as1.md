---
title: Labs - Tools and laziness
layout: margins
---

&nbsp;

## Tools and laziness

Here is a list of exercises related to project management,
testing, and laziness.

* [Packaging](#packaging)
* [Smooth permutations](#smooth-permutations)
<!-- * [Heap profiles](#heap-profiles) -->
<!-- * [Forcing evaluation](#forcing-evaluation) -->

### Packaging

The file [`Game.hs`](Game.hs) implements a small guessing game, all in one file. You can run it using `runghc Game.hs` on the terminal prompt. The goal of this first exercise is to turn this file into a proper Cabal project:

1. Initialize a project with one executable stanza.
2. Separate the pure part of the game (data type declarations and functions `next` and `step`) into a separate module, which should be imported by the `Main` module.
3. If you use Stack, initialize also the `stack.yaml` file.
4. Run the game using Cabal or Stack.

### Smooth permutations

In this assignment we want to build a library to generate smooth permutations. Given a list of integers `xs` and an integer `d`, a _smooth permutation of `xs` with maximum distance `d`_ is a permutation in which the difference of any two consecutive elements is at most `d`. A naïve implementation just generates all the permutations of a list,

```haskell
split []     = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

perms []     = [[]]
perms xs     = [(v:p) | (v, vs) <- split xs, p <- perms vs]
```

and then filters out those which are smooth,

```haskell
smooth n (x:y:ys) = abs (y - x) < n && smooth n (y:ys)
smooth _ _        = True

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)
```

**Exercise 1:** *Packaging and documentation*

1. Create a library `smoothies` which exports `perms` and `smoothPerms` from a module `SmoothPermsSlow`. You should be able to build the package by just running `cabal build` in the top level directory.
2. Document the exported functions using [Haddock](http://haskell-haddock.readthedocs.io/en/latest/index.html).

**Exercise 2:** *Testsuite*

1. Write a `SmothPermsTest` module with a comprehensive set of properties to check that `smoothPerms` works correctly.
2. Integrate your testsuite with Cabal using `tasty` ([here is how you do so](https://github.com/feuerbach/tasty#project-organization-and-integration-with-cabal)).

**Exercise 3:** *Implementation with trees*

The initial implementation of `smoothPerms` is very expensive. A better approach is to build a tree, for which it holds that each path from the root to a leaf corresponds to one of the possible permutations, next prune this tree such that only smooth paths are represented, and finally use this tree to generate all the smooth permutations from. Expose this new implementation in a new `SmoothPermsTree` module.

1. Define a data type `PermTree` to represented a permutation tree.
2. Define a function `listToPermTree` which maps a list onto this tree.
3. Define a function `permTreeToPerms` which generates all permutations represented by a tree.

    At this point the `perms` functions given above should be the composition of `listToPermTree` and `permTreeToPerms`.

4. Define a function `pruneSmooth`, which leaves only smooth permutations in the tree.
5. Redefine the function `smoothPerms`.

Integrate this module in the testsuite you developed in the previous exercise.

**Exercise 4:** *Unfolds*

Recall the definition of `unfoldr` for lists,

```haskell
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x = case next x of
                   Nothing     -> []
                   Just (y, r) -> y : unfoldr next r
```

We can define an unfold function for binary trees as well:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x = case next x of
                      Left  y      -> Leaf y
                      Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)
```

Define the following functions in a new module `UnfoldUtils`, which should *not* be exposed by your package. Define the functions using `unfoldr` or `unfoldTree`, as required.

1. `iterate :: (a -> a) -> a -> [a]`. The call `iterate f x` generates the infinite list `[x, f x, f (f x), ...]`.
2. `map :: (a -> b) -> [a] -> [b]`.
3. `balanced :: Int -> Tree ()`, which generates a balanced binary tree of the given height.
4. `sized :: Int -> Tree Int`, which generates any tree with the given number of nodes. Each leaf in the returned tree should have a unique label.

Define a new module `SmoothPermsUnfold` with an `unfoldPermTree` function which generates a `PermTree` as defined in the previous exercise. Then use that `unfoldPermTree` to implement a new version of `listToPermTree` and `smoothPerms`.

**(Optional)** Write the following proofs as comments in the `UnfoldUtils` module.

1. Prove using induction and equational reasoning that the version of `map` you defined using `unfoldr` coincides with the definition of `map` by recursion.
2. We define the `size` of a binary tree as the number of internal nodes.

    ```haskell
    size (Leaf _)   = 0
    size (Node l r) = 1 + size l + size r
    ```

    What is the `size` of a balanced tree as generated by `balanced`? Prove your result using induction and equational reasoning.

**Exercise 5:** *Performance*

1. Use the `criterion` package to make and run benchmarks for the given naïve
    solution and the implementations using trees and unfolds,
    in order to find out whether your solution really gives higher performance.
2. Use heap profiles to analyse and draw conclusions about the differences.

<!-- ### Heap profiles -->

<!-- **Exercise 1:** Generate heap profiles for the following functions: -->

<!-- ```haskell -->
<!-- rev  = foldl (flip (:)) [] -->
<!-- rev' = foldr (\x r -> r ++ [x]) [] -->
<!-- ``` -->

<!-- by using them as function `f` in a main program as follows -->

<!-- ```haskell -->
<!-- main = print $ f [1 .. 1000000] -->
<!-- ``` -->

<!-- (adapt the size of 1000000 according to the speed of your machine to get good -->
<!-- results). Interpret and try to explain the results! -->

<!-- **Exercise 2:** Do the same for -->

<!-- ```haskell -->
<!-- conc xs ys = foldr (:) ys xs -->
<!-- conc'      = foldl (\k x -> k . (x:)) id -->
<!-- ``` -->

<!-- with -->

<!-- ```haskell -->
<!-- main = print $ f [1 .. 1000000] [1 .. 1000000] -->
<!-- ``` -->

<!-- **Exercise 3:** Finally, have a look at -->

<!-- ```haskell -->
<!-- f1 = let xs == [1 .. 1000000] in if length xs > 0 then head xs else 0 -->
<!-- f2 = if length [1 .. 1000000] > 0 then head [1 .. 1000000] else 0 -->
<!-- ``` -->

<!-- ### Forcing evaluation -->

<!-- Write a function -->

<!-- ```haskell -->
<!-- forceBoolList :: [Bool] -> r -> r -->
<!-- ``` -->

<!-- that completely forces a list of booleans without using `seq`.  -->
<!-- Note that pattern matching drives evaluation. -->

<!-- Explain why the function `forceBoolList` has the type as specified above and not -->

<!-- ```haskell -->
<!-- forceBoolList :: [Bool] -> [Bool] -->
<!-- ``` -->

<!-- and why `seq` is defined as it is, and -->

<!-- ```haskell -->
<!-- force :: a -> a -->
<!-- force a = seq a a -->
<!-- ``` -->

<!-- is useless. -->
