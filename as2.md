---
title: Labs - Monads, transformers, applicatives
layout: margins
---

&nbsp;

## Monads, transformers, applicatives

Here is a list of exercises related to monads, monad transformers,
applicatives, foldables, and traversables.

* [Some instances](#some-instances-and-utilities)
* [Monads for a gambling game](#monads-for-a-gambling-game)
* [Instrumented `State` monad](#instrumented-state-monad)
* [Parsing with error messages](#parsing-with-error-messages)
  using monad transformers
* [Teletype IO](#teletype-io)

### Some instances and utilities

Given the standard type classes for functors, applicative functors and
monads:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  
class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b
```

Give instances for all three classes for the following data types:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
```

#### Foldable and traversable

Also give instances for the `Foldable` and `Traversable` classes, 
whenever possible:

```haskell
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m 
  
class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
```

#### Maps and keys

Using only methods from the above type classes and `lookup`, show how to
define the following function:

```haskell
lookupAll :: Ord k => [k] -> Data.Map k v -> Maybe [v]
```

This should return `Just vs` if all the argument keys occur in the
map, and `Nothing` otherwise.

Also define the following variant:

```haskell
lookupSome :: Ord k => [k] -> Data.Map k v -> [v]
```

that returns the list of values for which a key exists. You may want
to use functions from `Data.Maybe` to complete this definition.

#### Filter

Use `foldMap` to define a generic filter function:

```haskell
gfilter :: Foldable f => (a -> Bool) -> f a -> [a]
```

### Monads for a gambling game

Here's a game I like to play:  I toss a coin six times and count the number of
heads I see, then I roll a dice; if the number of eyes on the dice is greater
than or equal to the number of heads I counted then I win, else I lose. 
As I'm somewhat of a sore loser, I'd  like to know my chances of winning
beforehand, though. There are three ways to compute this probability:

1. Use a pen, paper (or, if you prefer, chalk and a blackboard) and some basic
   discrete probability theory to calculate the probability directly.
2. Draw or compute the complete decision tree of the game and count the number
   of wins and losses.
3. Write a computer program that simulates the game to approximate the probability.

We'll leave the first option to the mathematicians and focus on the second and
third possibilities. In fact, using monads, we'll see how both can be done at
the same time.

#### The `Gambling` Monad

Modeling a coin and a dice in Haskell shouldn't pose much difficulty for you:

```haskell
data Coin    = H | T
data Dice    = D1 | D2 | D3 | D4 | D5 | D6
data Outcome = Win | Lose
```

The tossing of a `Coin` and rolling of a `Dice` is given by the monadic interface
`MonadGamble`:

```haskell
class Monad m => MonadGamble m where
  toss :: m Coin
  roll :: m Dice
```

**Exercise 1:** Write a function `game :: MonadGamble m => m Outcome`
that implements the game above. Read the description of the game very carefully:
it is easy to make an off-by-one error; furthermore, as tossing and rolling are
side-effects the order in which you perform them matters.

#### Simulation

Simulating probabilistic events requires a (pseudo)random number generator.
Haskell has one available in the `System.Random` library. Random number
generators need to have access to a piece of state called the seed, as such
the random number generator runs in a monad, the `IO` monad to be exact.

**Exercise 2:** Give `Random` instances for `Coin` and `Dice`. (The `Random`
class has been largely superseded by other classes in the `random` package with
a different design, but `Random` is more low-tech and sufficient for our
purposes.)

**Exercise 3:** Give a `MonadGamble` instance for the `IO` monad.

**Exercise 4:** Write a function

```haskell
simulate :: IO Outcome -> Integer -> IO Rational
```

that runs a game of chance (given as the first parameter, not necessarily the
game implemented in Exercise 1) $n$ times($n > 0$, the second parameter)
and returns the fraction of games won. You can now approximate to probability
of winning using `simulate game 10000`.
Would you care to take a guess what the exact probability of winning is?

#### Decision trees
One drawback of simulation is that the answer is only approximate.  We can
obtain an exact answer using decision trees. Decision trees of probabilistic
games can be modeled as:

```haskell
data DecisionTree a = Result a | Decision [DecisionTree a]
```

In the leaves we store the result and in each branch we can take one of several
possibilities. As we don't store the probabilities of each decision, we'll have
to assume they are uniformly distributed (i.e., each possibility has an equally
great possibility of being taken). Fortunately for us, both fair coins and fair
dice produce a uniform distribution.

**Exercise 5:** Give a `Monad` instance for `DecisionTree`. (Hint: use the types
of `(>>=)` and `return` for guidance: it's the most straightforward,
type-correct definition that isn't an infinite loop.

**Exercise 6:** Give a `MonadGamble` instance for `DecisionTree`.

**Exercise 7:** Write a function

```haskell
probabilityOfWinning :: DecisionTree Outcome -> Rational
```

that, given a decision tree, computes the probability of winning. You can find
the exact probability of winning using `probabilityOfWinning game`. Was
your earlier guess correct? If you know a bit of probability theory, you can
double check the correctness by doing the pen-and-paper calculation suggested above.
Note that we used the same implementation of `game` to obtain both an approximate and
an exact answer.

### Instrumented `State` monad

A state monad is monad with additional monadic operations `get` and `put`:

```haskell
class Monad m => MonadState m s | m -> s where
  get    ::             m s
  put    :: s        -> m ()
  modify :: (s -> s) -> m s
```

Apart from the usual three monad laws, state monads should also satisfy:

```haskell
put s1 >> put s2               == put s2
put s  >> get                  == put s >> return s
get    >>= put                 == return ()
get    >>= (\s -> get >>= k s) == get >>= (\s -> k s s)
```

Check to see if you understand what these four laws say and if they make sense.

**Exercise 1:** Give default implementations of `get` and `put` in terms of
`modify`, and a default implementation of `modify` in terms of `get` and `put`.

#### Instrumentation

We are now going to define our own, slightly modified state monad that, besides
keeping track of a piece of state, has also been instrumented to count the
number of `(>>=)`, `return`, `get` and `put` operations that have been performed
during a monadic computation. The counts are given by the type:

```haskell
data Counts = Counts { binds   :: Int
                     , returns :: Int
                     , gets    :: Int
                     , puts    :: Int
                     }
```

**Exercise 2:** As a convenience, give a `Monoid` instance for `Count`
that sums the counts pairwise. Define constants

```haskell
oneBind, oneReturn, oneGet, onePut :: Counts
```

that represent a count of one `(>>=)`, `return`, `get` and `put` operation,
respectively.

Our state transformer is now given by:

```haskell
newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }
```

In addition to the usual state `s`, we keep track of the `Counts`
as an internal piece of state that is not exposed through the `get`
and `put` interface.

**Exercise 3:** Give `Monad` and `MonadState` instances for `State'`
that count the number of `(>>=)`, `return`, `get` and `put` operations.

#### Tree labeling

Here is a data type for binary trees that store values on the internal nodes only.

```haskell
data Tree a = Branch (Tree a) a (Tree a) | Leaf
```

**Exercise 4:** Write a function

```haskell
label :: MonadState m Int => Tree a -> m (Tree (Int, a))
```

that labels a tree with integers increasingly, using a depth-first in-order traversal.

**Exercise 5:** Write a function

```haskell
run :: State' s a -> s -> (a, Counts)
```

that runs a state monadic computation in the instrumented state monad, given
some initial state of type `s`, and returns the computed value and the number of
 operations counted. For example, the expression

```haskell
let tree = Branch (Branch Leaf "B" Leaf) "A" Leaf
in  run (label tree) 42
```

should evaluate to

```haskell
( Branch (Branch Leaf (42, "B") Leaf) (43, "A") Leaf
, Counts { binds = 10, returns = 5, gets = 4, puts = 2 } )
```

### Parsing with error messages

Instead of *backtracking* parsers covered in the lectures,
we can also define the following parser type:

```haskell
newtype ErrorMsg = ErrorMsg String
newtype Parser a = Parser (String -> Either ErrorMsg (a,String))
```

A parser consists of a function that reads from a `String` to produce
either an error message or a result of type `a` and the remaining
`String` that has not been parsed. This parser type does not allow
*backtracking* and is less expressive than the list-based parsers.

**Exercise 1:** 
Write the `Functor`, `Applicative`, `Monad`, and `Alternative` instances
for the parser type above.

**Exercise 2:**
Describe the `Parser` type as a series of monad transformers.

### Teletype IO

Consider the following data type:

```haskell
data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)
```

A value of type `Teletype` can be used to describe programs that read and write characters and return a final result of type `a`. Such a program can end immediately (`End`).  If it reads a character, the rest of the program is described as a function depending on this character (`Get`).  If the program writes a character (`Put`), the value to show and the rest of the program are recorded.

For example, the following expression describes a program that continuously echo characters:

```haskell
echo = Get (\c -> Put c echo)
```

**Exercise 1.** Write a `Teletype`-program `getLine` which reads characters until it finds a newline character, and returns the complete string.

A map function for `Teletype` can be defined as follows:

```haskell
instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)
```

**Exercise 2.** Define sensible `Applicative` and `Monad` instances for `Teletype`.

The definition of `Teletype` is not directly compatible with `do` notation. Usually, you have `getChar` and `putChar` primitives which allow you to write instead:

```haskell
echo = do c <- getChar
          putChar c
          echo
```

**Exercise 3.** Define those functions `getChar :: Teletype Char` and `putChar :: Char -> Teletype ()`.

**Exercise 4.** Define a [`MonadState`](https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Class.html#t:MonadState) instance for `Teletype`. How is the behavior of this instance different from the usual `State` type?

**Exercise 5.** A `Teletype`-program can be thought as a description of an interaction with the console. Write a function `runConsole :: Teletype a -> IO a` which runs a `Teletype`-program in the `IO` monad. A `Get` should read a character from the console and `Put` should write a character to the console.

One of the advantages of separating the description of `Teletype`-programs from their executions is that we can *interpret* them in different ways. For example, the communication might take place throught a network instead of console. Or we could mock user input and output for testing purposes.

**Exercise 6.** Write an interpretation of a `Teletype`-program into the monad `RWS [Char] () [Char]` ([documentation](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-RWS-Lazy.html)). In other words, write a function,

```haskell
type TeletypeRW = RWS [Char] () [Char]
runRWS :: Teletype a -> TeletypeRW a
```

Using it, write a function `mockConsole :: Teletype a -> [Char] -> (a, [Char])`.
