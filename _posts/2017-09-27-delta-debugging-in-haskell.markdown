---
layout: post
author: Michael Burge
title: "Debugging C with Haskell's Divisible"
started_date: 2017-09-02 06:18:00 -0700
date: 2017-09-27 06:32:00 -0700
tags:
  - haskell
---

A good type system covers a lot of the small bugs. Unit tests, careful design, and a sound mind can get you through some bigger ones. But sometimes complex bugs in large codebases call for heavy-duty debugging tools.

One such tool is __Delta Debugging__, which repeatedly shrinks recursive data structures to find a minimal counterexample that still exhibits the bug. You'll appreciate the technique if you've ever used `git bisect` to locate a small breaking change in a large codebase.

This article:

* Introduces the Delta Debugging technique
* Implements a general-purpose delta-debugging tool
* Uses Haskell's FFI to control a C chess engine
* Locates an error introduced into the chess engine

## The Setup

In [my last article](http://www.michaelburge.us/2017/09/10/injecting-shellcode-to-speed-up-amazon-redshift.html), I implemented a chess engine in C. I've introduced an error into its move generator. Recall that the move generation involved 5 functions:

{% highlight cpp %}
typedef ... gamestate;
typedef ... iterator;
typedef ... move;

// Move generation
iterator mkIterator(gamestate g);
iterator advance_iterator(gamestate g, iterator i);
gamestate apply_move(gamestate g, move m);
bool is_iterator_finished(iterator x);
move dereference_iterator(iterator i);


void print_fen(gamestate g, char *buffer);
gamestate new_game();

// The actual function used to define the bug
uint64_t perft(gamestate g, int depth);
{% endhighlight %}

The "gold standard" for testing a chess engine is to compare the output of the above `perft` function with [published reference values](https://chessprogramming.wikispaces.com/Perft+Results). The bug that we'll be finding is easily visible from the starting state of the board, so `new_game` will be the only `gamestate` value I'll expose from the engine.

In the FFI section, I'll define Haskell wrappers for everything:
{% highlight haskell %}
data Gamestate
data Iterator
data Move

newGame             :: Gamestate
printFen            :: Gamestate -> String
mkIterator          :: Gamestate -> Iterator
advanceIterator     :: Gamestate -> Iterator -> Iterator
perft               :: Gamestate -> Int32    -> Word64
applyMove           :: Gamestate -> Move     -> Gamestate
isIteratorFinished  :: Iterator  -> Bool
dereferenceIterator :: Iterator  -> Move
{% endhighlight %}

To detect the bug, we'll need a reference version of `perft` to compare ours against. I'll use [Roce Chess](http://www.rocechess.ch/perft.html), so our Haskell will literally invoke a `roce38` process, set the board state, and ask it to calculate a `perft` value.
{% highlight haskell %}
type Move' = String

reference_perft :: Gamestate -> Int32 -> Word64
reference_moves :: Gamestate -> [Move']
{% endhighlight %}

I've removed the `IO` constraint using `unsafePerformIO` to simplify the article's progression, but the final solution will be general enough to work over `IO`.

## Reducible

Here's a variation on [QuickCheck's](https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck-Arbitrary.html) `Arbitrary` typeclass:

{% highlight haskell %}
-- | Gives all of the single-step reductions for a type.
-- | It is recommended to put the largest reductions first.
class Reducible a where
  reductions :: a -> [a]

minimize :: Reducible a => (a -> Bool) -> a -> a
minimize f x = case find f $ reductions x of
  Nothing -> x
  Just y -> minimize f y
{% endhighlight %}

If you have an HTML file that crashes your web browser, `minimize` would remove tags or characters until it is a minimal crash-causing file. This variant would remove only a single tag or character at a time, while `Arbitrary` removes entire batches for performance.

Recall that `perft` takes a `Gamestate` and a `Depth`. The "smallest possible" means that the `Depth` is the smallest.
{% highlight haskell %}
instance Reducible Gamestate where
  reductions :: Gamestate -> [Gamestate]
  reductions g =
    let loop i =
          if isIteratorFinished i
          then []
          else
            let m = dereferenceIterator i
                g' = applyMove g m
                i' = advanceIterator g i
            in g' : loop i'
    in loop $ mkIterator g

type Depth = Int32
newtype PerftTest = PerftTest (Gamestate, Depth) deriving (Show)

instance Reducible PerftTest where
  reductions :: PerftTest -> [PerftTest]
  reductions (PerftTest (g, depth)) =
    if depth == 1
    then []
    else map (\g' -> PerftTest (g', depth-1)) $ reductions g
{% endhighlight %}

And here's how to apply this to find a specific position and move that differ between the two engines:

{% highlight haskell %}
module Main where

import Chess
import Reducible

checkBug :: PerftTest -> Bool
checkBug (PerftTest (g, depth)) =
  perft g depth /= reference_perft g depth

compareMoves :: Gamestate -> ([Move'], [Move'])
compareMoves g =
  let lefts = map printMove $ moves g
      rights = reference_moves g
  in (lefts \\ rights, rights \\ lefts)

instance Show Gamestate where
  show g = printFen g

main :: IO ()
main = do
  let g = newGame
      i = mkIterator g
  let (PerftTest (g', depth)) = minimize checkBug $ PerftTest (g, 3)
  putStrLn $ show $ g'
  putStrLn $ show $ compareMoves g'
  
{- Prints:
r1bqkbnr/pppppppp/n7/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1
([],["e1e2"])
-}
{% endhighlight %}

The FEN string can be pasted directly into a program like `XBoard` to visualize the position, and "e1e2" is a move that Rocechess emits that my engine doesn't.

At this point, you would have to dig into the C to see why the King at e1 is not being allowed to move northward to e2. I'll spoil that by revealing the change that I made:
{% highlight cpp %}
private uint64_t valid_king_moves(gamestate g, int idx)
{
  uint64_t ret =
    bit(move_direction(idx, DIRECTION_EAST)) |
    bit(move_direction(idx, DIRECTION_WEST)) |
    // bit(move_direction(idx, DIRECTION_NORTH)) |
    bit(move_direction(idx, DIRECTION_SOUTH)) |
    bit(move_direction(idx, DIRECTION_NORTHEAST)) |
    bit(move_direction(idx, DIRECTION_SOUTHEAST)) |
    bit(move_direction(idx, DIRECTION_NORTHWEST)) |
    bit(move_direction(idx, DIRECTION_SOUTHWEST))
    ;
  ...
  return ret;
}
{% endhighlight %}

My experience across many different software projects is that most of the work in debugging is in creating a small isolated test case that demonstrates the problem, and not in the actual fix.

## Divisible

[`Divisible`](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html) is the main reason I wrote this article, and allows us to generalize `minimize` above. Since the context is debugging a chess engine, I'll use the language of deductive logic to describe how we'll use it.

The idea is that to prove that the value of `perft g 6` is correct, you need two components:

* A way to split `perft g 6` into subproblems `perft g' 5`.
* A way to prove base cases correct: `perft g 0 == 1`

Since we're debugging, such a proof is impossible because we have a failing testcase in-hand. However, we get to define what "proof" means in our context, so we'll use "Correct, or produces a concrete minimal counterexample":

{% highlight haskell %}
-- A testcase a can be decomposed into smaller child problems.
-- A Counterexample finder scans an a and produces:
-- 1. Nothing: A proof that all children are correct
-- 2. Just b: Evidence of type b that some child is erroneous
newtype Cx b a = Cx { unCx :: a -> Maybe b }
{% endhighlight %}

There are actually 3 different typeclasses, corresponding to 3 different proof techniques:
* [`Contravariant`](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html) generalizes [Modus Tollens](https://en.wikipedia.org/wiki/Modus_tollens).
* [`Divisible`](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html#t:Divisible) generalizes [Conjunction Introduction](https://en.wikipedia.org/wiki/Conjunction_introduction).
* [`Decidable`](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html#t:Decidable) generalizes [Disjunction Elimination](https://en.wikipedia.org/wiki/Disjunction_elimination).

### Contravariant

In most programs, you start with values obtained from sensory data like files or system calls, and apply functions to convert those into more abstract or useful information. This is reversed when debugging: We start with a goal in mind(a failing testcase), and work "backwards" to find the first place with a problem. Thus, `Contravariant`, `Divisible`, and `Decidable` have type signatures that seem "impossible" in forward-thinking programs:
{% highlight haskell %}
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
{% endhighlight %}

In a proof context, `contramap` is a way of reducing a goal of type `a` to a goal of type `b`: If `f b` represents a sensor that detects erroneous `b` values, then we can convert it into a sensor that detects erroneous `a` values by converting the `a`s to `b`s and then scanning the `b`s.

A goal will usually have a function type like `type Goal a = a -> Bool`. The mystery behind `Contravariant`'s impossible signature goes away when you realize we don't have a value of type `a` or `b`  in the first place: We only have a function, so we're waiting on code upstream to deliver one to us.

Here's a concrete implementation
{% highlight haskell %}
instance Contravariant (Cx b) where
  contramap f (Cx g) = Cx (g . f)
{% endhighlight %}

### Divisible

`contramap` lets us solve a goal by relating it to an easier goal, and then solving the easier one. We might also want to split the goal into multiple easier subgoals:

{% highlight haskell %}
class Contravariant f => Divisible f where
  divide :: (a -> (b, c)) -> f b -> f c -> f a
  conquer :: f a
{% endhighlight %}

`divide` is reasonable enough, but in a proof context `conquer` seems to let us prove anything at all. `divide` can be generalized to arbitrary tuples, and `conquer` is the empty conjuction:
{% highlight haskell %}
divide4 :: Divisible f     => (a -> (b,c,d,e)) -> f b -> f c -> f d -> f e -> f a
divide3 :: Divisible f     => (a -> (b,c,d)) -> f b -> f c -> f d -> f a
divide2 :: Divisible f     => (a -> (b,c)) -> f b -> f c -> f a
divide1 :: Contravariant f => (a -> (b)) -> f b -> f a
divide0 :: Divisible f     => (a -> ()) -> f a

divide0 _ = conquer
divide1 = contramap
divide2 = divide
divide3 split3 pb pc pd =
  divide (reassociate . split3) pb $
  divide id pc pd
  where
    reassociate (x,y,z) = (x,(y,z))

divide4 split4 pb pc pd pe =
  divide (reassociate4 . split4) pb $
  divide3 id pc pd pe
  where
    reassociate4 (x,y,z,w) = (x,(y,z,w))
{% endhighlight %}

In general, `divide` takes a collection of subproblems and solutions for those subproblems, and combines them into a solution for the larger problem. Here's the tuple example generalized to arbitrarily-large lists:
{% highlight haskell %}
divideList :: Divisible f => [(a -> b, f b)] -> f a
divideList = \case
  []           -> conquer
  ((f, px):xs) -> divide (reproject f) px (divideList xs)
  where
    reproject :: (a -> b) -> (a -> (b,a))
    reproject f = \x -> (f x, x)
{% endhighlight %}

The `divideList` above isn't fully-general because it requires every subproblem to be of the same type.

Then, `conquer` is an assertion that no further subproblems need to be checked. Recall that our `Cx` type means "Either all children are correct, or we've found suspicious evidence". `conquer` will correspond to this first case.

Here's how to implement `divide` for our counterexample detector:
{% highlight haskell %}
instance Divisible (Cx b) where
  divide split (Cx left) (Cx right) = Cx $ \x ->
    let (lProblem, rProblem) = split x
    -- One branch must produce a counterexample
    in case (left lProblem, right rProblem)  of
      (Just x, _) -> Just x
      (_, Just x) -> Just x
      (Nothing, Nothing) -> Nothing
  conquer = Cx $ const Nothing
{% endhighlight %}

Since both subproblems must be clean in order for the parent to be clean, we can can return early if we find an error in the left branch.

### Decidable

`contramap` corresponds to the logical [contrapositive](https://en.wikipedia.org/wiki/Proof_by_contrapositive), while `divide` corresponds to conjuction. The other major tool is disjunction, which is provided by `choose`.
{% highlight haskell %}
class Divisible f => Decidable f where
  lose   :: (a -> Void) -> f a
  choose :: (a -> Either b c) -> f b -> f c -> f a
{% endhighlight %}

`lose` is a placeholder when you know that a branch in `choose` is impossible yet are required to provide a proof. There are only 3 possible implementations:
* An exception, using `undefined` or [`absurd`](https://hackage.haskell.org/package/void-0.7.1/docs/Data-Void.html).
* `const conquer`
* A constant distinct from `conquer`

I don't see any examples of the 3rd type in the `contravariant` package, but I imagine that it could be used to write backwards backtracking algorithms similarly to how [`guard :: Bool -> [()]`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Monad.html#v:guard) is used to [prune branches](https://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus.html) using `MonadPlus`.

`choose` in our context says that if you are trying to locate errors in a value of type `a`, can split it into one of two subproblems, and can locate errors in both subproblems, then you can locate errors by figuring out which subproblem is appropriate for `a` and reusing its proof. Here's the implementation:
{% highlight haskell %}
instance Decidable (Cx b) where
  lose _ = conquer
  choose split left right = Cx $ \x ->
    -- Both branches must produce a counterexample
    case split x of
      Left l -> unCx left l
      Right r -> unCx right r
{% endhighlight %}

The main difference between `Decidable` and `Divisible` is that `Decidable` branches to precisely one of its subproblems, while `Divisible` combines subproblems together.

I'll implement `decideList` to give a feel for how `Decidable` works in practice before moving on to reimplement `minimize`.

{% highlight haskell %}
decideList :: forall a b f. Decidable f => (a -> (Integer, b)) -> [f b] -> f a
decideList f (xp:xps) = choose caseConstructor xp (decideList nextF xps)
  where
    caseConstructor :: a -> Either b a
    caseConstructor x =
      let (c, w) = f x
      in case c of
        0 -> Left w
        n -> Right x
    nextF :: a -> (Integer, b)
    nextF x = let (c, w) = f x
              in (c-1, w)
{% endhighlight %}

The idea here is that `(Integer, b)` consists of a branch selector and a value `b`. For `Either`, `Left` would be branch 0, while `Right` would be branch 1. `caseConstructor` either chooses the current branch, or threads the `a` value along to the next one.

As with `divideList`, `decideList` isn't fully-general because it requires every subproblem to be of the same type.

### minimize

We're ready to generalize our original `minimize` function. Here it is again:
{% highlight haskell %}
minimize :: Reducible a => (a -> Bool) -> a -> a
minimize f x = case find f $ reductions x of
  Nothing -> x
  Just y -> minimize f y
{% endhighlight %}

We generalize it in 3 steps:
* First, our `Cx` type contains an `a -> Maybe a`, which can represent both the `a -> Bool` and `a -> a` portions of `minimize`. So `minimize` becomes `Reducible a => Cx a a -> Cx a a`.

* Second, I've generalized `Cx` to a `Decidable f` constraint, causing its signature to be `(Decidable f, Reducible a) => f a -> f a`.

* Third, `minimize` calls [`find :: (a -> Bool) -> [a] -> Maybe a`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-List.html). So I've generalized it to `findD :: Decidable f => f a -> f [a]`.

{% highlight haskell %}
import Data.List (uncons)
import Control.Arrow ((&&&))

minimizeD :: (Decidable f, Reducible a) => f a -> f a
minimizeD pred = divide (reductions &&& id) (findD $ minimizeD pred) pred

findD :: Decidable f => f a -> f [a]
findD p = chooseMaybe uncons conquer (divided p (findD p))

chooseMaybe :: Decidable f => (a -> Maybe b) -> f () -> f b -> f a
chooseMaybe p nothing just = choose (maybe (Left ()) Right . p) nothing just

uncons :: [a] -> Maybe (a, [a])
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b,c))
{% endhighlight %}

In our specific context, here's how you might read these in English:
* `minimizeD`: A minimal error is either in one of the `reductions` from the current node, or the current node itself(`id`).
* `findD`: A minimal error is either nonexistent because the list was empty(`conquer`), the current element(`p`), or in the remainder of the list(`findD p`).

And here's a function with a simpler type that can be easily applied:
{% highlight haskell %}
minimizeD'' :: PerftTest -> Maybe PerftTest
minimizeD'' x = unCx (minimizeD (Cx $ liftPred checkBug)) x
  where
    liftPred :: (a -> Bool) -> (a -> Maybe a)
    liftPred pred = \x ->
      if pred x
      then Just x
      else Nothing

testCase :: Int -> PerftTest
testCase n = PerftTest (newGame, n)

exampleInvocation = minimizeD'' $ testCase 3
{% endhighlight %}

### Performance

One of the reasons I wrote an FFI to a C chess engine was to demonstrate that the terms and typeclasses I'm pushing around aren't just abstract nonsense but have actual computational meaning. Nobody criticizes C for being too abstract, so I felt that it would give a good counterbalance to the control code in `minimizeD`.

Recall that `pred` instantiates a separate system process every time it's called. It seems reasonable that a good solution should create as few subprocesses as possible. Even ignoring my use of `unsafePerformIO`, a good solution should avoid recursing too deeply or invoking expensive functions unnecessarily.

I'll add a global counter that we increment every time we create a Rocechess process. I'll use the counter to detect any hidden performance issues.

{% highlight haskell %}
import Data.IORef

counter = unsafePerformIO $ newIORef 0

tickCounter :: IO ()
tickCounter = modifyIORef' counter (+1)

timeTicks :: Show a => a -> IO Int
timeTicks x = do
  ticks <- readIORef counter
  putStrLn $ show x
  ticks' <- readIORef counter
  return $ ticks' - ticks
{% endhighlight %}

`timeTicks` forces its input by printing it as output, and compares the counter before and after. Here's the output using the original `minimize`:
{% highlight haskell %}
timeTicks $ minimize checkBug $ testCase 3
-- PRINTS
-- PerftTest (r1bqkbnr/pppppppp/n7/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1,1)
-- 14
{% endhighlight %}

So a total of 14 processes are spawned calculating reference `perft` values to find this minimal testcase. How does our slick one-liner fare?
{% highlight haskell %}
timeTicks $ minimizeD'' $ testCase 3
-- PRINTS
-- Just (PerftTest (r1bqkbnr/pppppppp/n7/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1,1))
-- 253
{% endhighlight %}

The generalized algorithm generates 18x as many calls as our simple one, to find the same failing game position.

### Refinable

To solve this problem, I'll first diagnose the problem and then introduce a new typeclass `Refinable` to solve it.

Let's compare the two side-by-side:
{% highlight haskell %}
minimizeD :: (Decidable f, Reducible a) => f a -> f a
minimizeD pred = divide (reductions &&& id) (findD $ minimizeD pred) pred

minimize :: Reducible a => (a -> Bool) -> a -> a
minimize f x = case find f $ reductions x of
  Nothing -> x
  Just y -> minimize f y
{% endhighlight %}

Notice that `minimize` calls `f` on each element emitted by `reductions`, while `minimizeD` calls `minimizeD pred`. With `minimize`, there is no recursion if the node evaluates to `Nothing`, since we know all children are correct.

We could stop the recursion by only passing `pred` to `findD`:
{% highlight haskell %}
 
minimizeD :: (Decidable f, Reducible a) => f a -> f a
minimizeD pred = divide (reductions &&& id) (findD $ pred) pred
{% endhighlight %}

But now the counterexample is only 1 step smaller, and not a minimal counterexample.

What we need is to __refine__ the failure case. First we run `pred` to scan for an element in a parent node(a __coarse__ failure), then we run `minimizeD pred` to reduce it once we've confirmed that one is present(a __fine__ failure).

Here's how to implement that:
{% highlight haskell %}
class Divisible f => Refinable f where
  implies :: f a -> f a -> f a

instance Refinable (Cx b) where
  implies l r = Cx $ \x ->
    case unCx l x of
      Nothing -> Nothing
      Just _ -> unCx r x
{% endhighlight %}

Just as `divide` lets us abort a proof as soon as we find a counterexample, `implies` lets us abort a proof as soon as we prove that no counterexample exists:
{% highlight haskell %}
implies conquer x = conquer

divide delta conquer x = x
divide delta x conquer = x

delta x = (x,x)
{% endhighlight %}

Both optimizations are used here: The first in `findD` to abort searching when we find an element, and the second we'll add explicitly:
{% highlight haskell %}
minimizeD :: (Refinable f, Decidable f, Reducible a) => f a -> f a
minimizeD pred = divide (reductions &&& id) (findD checkChild) pred
  where
    checkChild = pred `implies` minimizeD pred
{% endhighlight %}

Now the numbers are almost equal:
{% highlight haskell %}
PerftTest (r1bqkbnr/pppppppp/n7/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1,1)
14
*Main> timeTicks $ minimizeD'' $ testCase 3
Just (PerftTest (r1bqkbnr/pppppppp/n7/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1,1))
15
{% endhighlight %}

The extra node is because `minimizeD` scans the root node, while `minimize` immediately recurses into it. Thus, we've inadvertently fixed a minor inconsistency in `minimize`.

## FFI

The last component is the interface between the C++ engine and the Haskell control code. To start with, here are C++ functions I'll expose:

{% highlight cpp %}
#include "chess.cpp"

extern "C" {

  void new_game_w(gamestate *g)
  { *g = new_game(); }

  void print_move_w(move *m, char *buffer)
  { print_move(*m, buffer); }
  
  void print_fen_w(gamestate *g, char *buffer)
  { print_fen(*g, buffer); }

  void mkIterator_w(gamestate *g, iterator *i)
  { *i = mkIterator(*g); }

  void advance_iterator_w(gamestate *g, iterator *i, iterator *result)
  { *result = advance_iterator(*g, *i); }

  uint64_t perft_w(gamestate *g, int depth)
  { return perft(*g, depth); }

  void apply_move_w(gamestate *g, move *m, gamestate *result)
  { *result = apply_move(*g, *m); }

  int is_iterator_finished_w(iterator *i)
  { return is_iterator_finished(*i) ? 1 : 0; }

  void dereference_iterator_w(iterator *i, move *m)
  { *m = dereference_iterator(*i); }
  
};
{% endhighlight %}

Since GHC doesn't support passing `struct` values on the stack, I allocate temporary memory and pass in a pointer for the C++ code to write to.
{% highlight haskell %}
newtype Gamestate = Gamestate BS.ByteString
newtype Iterator = Iterator BS.ByteString
newtype Move = Move BS.ByteString

instance Show Gamestate where
  show g = printFen g

instance Show Move where
  show m = printMove m

pokeBs :: Ptr a -> BS.ByteString -> IO ()
pokeBs ptr bs = BS.useAsCStringLen bs $ \(src, len) ->
  copyBytes (castPtr ptr) src len

instance Storable Gamestate where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = Gamestate <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Gamestate))
  poke ptr (Gamestate bs) = pokeBs ptr bs

instance Storable Move where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Move <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Move))
  poke ptr (Move bs) = pokeBs ptr bs

instance Storable Iterator where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = Iterator <$> BS.packCStringLen (castPtr ptr, sizeOf (undefined :: Iterator))
  poke ptr (Iterator bs) = pokeBs ptr bs
{% endhighlight %}

Each wrapped function follows a regular pattern of allocating memory, writing arguments to it, calling the function, and reading the result. For brevity, I'll only show a single one:

{% highlight haskell %}
applyMove :: Gamestate -> Move -> Gamestate
applyMove g m = unsafePerformIO $
  alloca $ \g_ptr ->
  alloca $ \m_ptr ->
  alloca $ \g'_ptr -> do
    poke g_ptr g
    poke m_ptr m
    applyMove_w g_ptr m_ptr g'_ptr
    peek g'_ptr
{% endhighlight %}

That covers my own chess engine. We also need to call out to Rocechess to use as a reference. Here's the function that does that:

{% highlight haskell %}
{- Standard output from the "roce38" process looks like:
Roce version: 0.0380 - Roman's Own Chess Engine
Copyright (C) 2003-2007 Roman Hartmann, Switzerland. All rights reserved.
warning: couldn't open Roce.cfg

roce: 
roce: 
Perft (3): 8902, Time: 0.001 s
-}
runRoceCommands :: [String] -> ([String] -> a) -> IO a
runRoceCommands commands parseOutput = do
    (Just hin, Just hout, _, ph) <- createProcess $ (proc "./roce38" []) {
      std_in = CreatePipe,
      std_out = CreatePipe,
      std_err = Inherit
      }
    hSetBuffering hout NoBuffering
    hSetBuffering hin NoBuffering
    forM_ commands $ \command -> do
      hPutStr hin command
      hPutChar hin '\n'
    output <- hGetContents hout
    tickCounter
    return $ parseOutput $ drop 6 $ lines output

reference_perft_w :: Gamestate -> Int32 -> IO Word64
reference_perft_w g d =
  let commands = [
        "setboard " ++ printFen g,
        "perft " ++ show d,
        "quit"
        ]
      parseOutput (perft_line : _) =
        let perft_word = splitOneOf " ," perft_line !! 2
            perft = read perft_word
        in perft
        
  in runRoceCommands commands parseOutput

reference_perft g d = unsafePerformIO $ reference_perft_w g d
{% endhighlight %}

Note the use of `tickCounter` to measure the number of times we invoke this expensive command.


## Why not Arbitrary?

I mainly used `Reducible` in this article because it is a simpler typeclass than `Arbitrary`. There's not much reason to use `Reducible` over `Arbitrary` for our chess example. This is because `perft` test failures are __monotone__: A successful test implies that all transitive children are successful. A real example that isn't monotone that I've come across would be a compiler that emitted a jump to a non-existent label:

{% highlight cpp %}
label1:
  goto label2
  goto label1 
{% endhighlight %}

In this case, we want `minimize` to return just `goto label2`, because that is the smallest statement that exhibits the bug. However, `shrinkList` in the `QuickCheck` package employs an optimization that drops an exponentially-decreasing percentage of elements from a list for performance, which means it could drop `label1` and `goto label2`:
{% highlight cpp %}
goto label1
{% endhighlight %}

This still has a jump to a non-existent label, but only because our shrinking dropped it. It's possible to restore the monotone property in full by defining the error condition in sufficient detail; but it can also be restored by only reducing to immediate children. It may be easier to check that `label1` can be excluded from the immediate children because `goto label1` depends on it, than to check whether an entire set of statements can be safely removed.

## Conclusion

We generalized a function for reducing a failing testcase to a minimal state. In particular, there is an instance for `Compose` allowing the resulting algorithm to work over any `Monad` or `Applicative`. This allows one to augment the reduction process with benchmarking information, experiment with clearly-defined optimizations, and test novel reduction algorithms on simple pure values which automatically generalize to the full application.

The code for this article is available on [Github](https://github.com/MichaelBurge/delta-debugging-example).

Future articles may cover:
* Further optimizing testcase reduction tools
* Generalizing other common utilities with abstract typeclasses
* More techniques for debugging programs
