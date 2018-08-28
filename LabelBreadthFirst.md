---
title: Breadth-first labelling
layout: margins
---

# Solution to the breadth first labelling problem

```haskell
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

-- the function bfl replaces the a's in a tree by consecutive values from the list [b] in a breadth-first way
bfl :: Tree a -> [b] -> Tree b

bfl t l = let bf Leaf lss = (Leaf, lss)
              bf (Node lt _ rt) ((l:ls):lls) = 
                   let (lr, ills) = bf lt lls
                       (rr, rlls) = bf rt ills
                   in (Node lr l rr, ls:rlls)
              (result, nlevels) = bf t (l:nlevels)
          in result

t :: Tree Int
t = Node (Node (Node Leaf 1 Leaf) 1 (Node Leaf 1 (Node Leaf 1  (Node Leaf 1 Leaf)))) 1 Leaf

{-
*Main> bfl t [1..]
Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 (Node Leaf 5 (Node Leaf 6 Leaf)))) 1 Leaf
-}
```

