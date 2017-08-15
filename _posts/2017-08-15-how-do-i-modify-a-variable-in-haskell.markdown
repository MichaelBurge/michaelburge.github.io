---
layout: post
author: Michael Burge
title: "How do I modify a variable in Haskell?"
published: true
started_date: 2017-08-13 09:20:00 -0700
date: 2017-08-15 03:36:00 -0700
tags:
  - haskell
---

Haskell programmers seem to get by without variable mutation, which is odd if you're used to C, Javascript, or another imperative language. It seems like you need them for loops, caches, and other state.

This article will go over many of the techniques I use for these purposes.

## The Problem

Suppose you want to print an identity matrix:

{% highlight perl %}
1000000000
0100000000
0010000000
0001000000
0000100000
0000010000
0000001000
0000000100
0000000010
0000000001
{% endhighlight %}

In an imperative language like Perl, you'll likely split this into 4 steps:

{% highlight perl %}
my $size = 10;
# 1. Declare the array, allocating memory if necessary.
my $array;

# 2. Initialize the array to 0
for (my $i = 0; $i < $size; $i++) {
    for (my $j = 0; $j < $size; $j++) {
        $array->[$i]->[$j] = 0;
    }
}

# 3. Set the diagonal to 1.
for (my $idx = 0; $idx < $size; $idx++) {
    $array->[$idx]->[$idx] = 1;
}
 
# 4. Print the array
for (my $i = 0; $i < $size; $i++) {
    for (my $j = 0; $j < $size; $j++) {
        my $value = $array->[$i]->[$j];
        print $value;
    }
    print "\n";
}
{% endhighlight %}

In Haskell, we run into trouble when we realize there's no built-in mutating assignment operation:

{% highlight haskell %}
import Data.Array

writeArray = undefined

size = 10
-- 1. Declare the array
arr :: Array (Int,Int) Integer
arr = array ((1,1), (size,size)) []

main :: IO ()
main = do
  -- 2. Initialize the array to 0
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ writeArray arr i j 0
  -- 3. Set the diagonal to 1
  sequence_ $ do
    i <- [1..size]
    return $ writeArray arr i i 1

  -- 4. Print the array
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ do
      putChar $ if arr ! (i,j) == 0
                then '0'
                else '1'
      if j == size
        then putChar '\n'
        else return ()
{% endhighlight %}

The expression `writeArray array i j k` is supposed to be equivalent to Perl's
{% highlight perl%}
$array->[$i]->[$j] = $k;
{% endhighlight %}

Each call to `sequence_` executes a list of actions. The `do` notation in List context is a way of stretching the list generator notation out to multiple lines: Step 2 above is equivalent to

{% highlight haskell %}
sequence_ [ writeArray arr i j 0 | i <- [1..10], j <- [1..10] ]
{% endhighlight %}

Notice that the indices are one-based(`[1..10]`) and not zero-based(`[0..9]`), because I declared the boundaries of the array to be `(1,1)` and `(10,10)` here:
{% highlight haskell %}
arr = array ((1,1), (size,size)) []
{% endhighlight %}

The code above compiles but fails to run because `writeArray` is not defined. There's no way to define `writeArray`, because values are immutable in Haskell. We use `writeArray` in 2 places: When we initialize the array, and when we modify the diagonal. We can initialize the array when we declare it, so that removes the first issue:

{% highlight haskell %}
import Data.Array

-- 1. Declare the array
size = 10
arr :: Array (Int,Int) Integer
arr = array ((1,1), (size,size)) $ do
  -- 2. Initialize the array to 0
  i <- [1..size]
  j <- [1..size]
  return ((i,j), 0)

printElement :: (Int, Int) -> IO ()
printElement (i,j) = do
  putChar $ if arr ! (i,j) == 0
            then '0'
            else '1'
  if j == size
    then putChar '\n'
    else return ()

main :: IO ()
main = do
  -- 4. Print the array
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ printElement (i,j)
{% endhighlight %}

In `arr` and `main`, the `do` is in List context because `sequence_` and `array` expect a List. In `printElement`, the `do` is in IO context, which allows us to define an action that chains together multiple dependent actions.

If you run this sample, you should see a 10x10 matrix with all zeroes:

{% highlight perl %}
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
{% endhighlight %}

Steps 1,2, and 4 are easy. Our problem is Step 3: We have no way to define `writeArray`, since the array is immutable.

## Mutable Values

If arrays are immutable, could we somehow make it mutable?

[`Data.IORef`](https://hackage.haskell.org/package/base/docs/Data-IORef.html) allows us to create and modify mutable garbage-collected values in IO context. This makes programming in IO context similar to a language like C# or Javascript, if a bit more verbose.

References are normally transparent in imperative languages: C++ has the concept of an lvalue and rvalue because reads and writes are not explicitly written out. Also, stack-allocated declarations look similar to assignments, so it's not clear that allocation is happening:

{% highlight c++ %}
#include <stdio.h>

int main() {
  int x = 5; // Allocation
  x = 10; // Write
  int y = x; // Read

  printf("%d\n", y);
}
{% endhighlight %}

Pointers make the allocation and indirect access a little more explicit. However, `*x` can involve either a read or write depending on the context:

{% highlight c++ %}
#include <stdio.h>

int main() {
  int *x = new int(5); // Allocation
  *x = 10; // Write
  int y = *x; // Read

  printf("%d\n", y);
}
{% endhighlight %}

Using `IORef`, that is equivalent to:
{% highlight haskell %}
import Data.IORef

main = do
  -- x :: IORef Int
  x <- newIORef 5
  writeIORef x 10
  -- y :: Int
  y <- readIORef x

  putStrLn $ show y
{% endhighlight %}

Like pointers, a reference to an integer is different from an integer. I've left commented-out type signatures above to show this.

* `newIORef` creates a mutable reference.
* `readIORef` reads the reference
* `writeIORef` changes it to point to a new value. (Old values are automatically garbage-collected.)

All of these need to be in IO context, because reads and writes can't be reordered. 

`modifyIORef'` will read the value using a reference, apply a function to it, and write the value back. You almost always want to use the strict version `modifyIORef'` rather than the lazy version `modifyIORef`, because it has more predictable performance.

{% highlight haskell %}
import Data.Array
import Data.IORef

type IntArray = Array (Int,Int) Integer

writeArray :: IORef IntArray -> (Int, Int) -> Integer -> IO ()
writeArray ref index value =
  modifyIORef' ref $ \arr ->
    arr // [(index, value)]

size = 10

printElement :: IORef IntArray -> (Int, Int) -> IO ()
printElement ref (i,j) = do
  arr <- readIORef ref
  putChar $ if arr ! (i,j) == 0
            then '0'
            else '1'
  if j == size
    then putChar '\n'
    else return ()


main :: IO ()
main = do
  -- 1. Declare the array
  -- ref :: IORef IntArray
  ref <- newIORef $ array ((1,1), (size,size)) $ do
    -- 2. Initialize the array to 0
    i <- [1..size]
    j <- [1..size]
    return ((i,j), 0)
  
  -- 3. Set the diagonal to 1
  sequence_ $ do
    i <- [1..size]
    return $ writeArray ref (i,i) 1
  -- 4. Print the array
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ printElement ref (i,j)
{% endhighlight %}

The expression `arr // [(index, value)]` is a new array with an updated value at index. It does not read or write to memory - `modifyIORef` will do the reading and writing - it simply is the new array. `[(index, value)]` is a list because `(//)` allows us to update multiple values at once, though I only update a single value each time here.

One shortcoming of this technique is that it requires your code to be in `IO` context: `main`, `printElement`, and `writeArray` all use `IO`. If you want to add mutable variables throughout your code, you'll have to change everything to be in IO context. If you want to actually mutate global variables, this is necessary; but see the `ST` monad below to avoid this if you only need mutation within a single function.

While this is the most direct way to do what we want, Haskellers usually get by without any mutation. How would that work?

## The Haskell Way

We know how to create new arrays, but updating them is tricky. So one way around this is to create a new array, and have later code use it. This is what I would likely write in real code:

{% highlight haskell %}
import Data.Array

type IntArray = Array (Int,Int) Integer

size = 10
-- 1. Declare the array
arr :: IntArray
arr = array ((1,1), (size,size)) $ do
  -- 2. Initialize the array to 0
  i <- [1..size]
  j <- [1..size]
  return ((i,j), 0)

-- 3. Modify the array
updateArray :: IntArray -> IntArray
updateArray arr = arr // [((i,i), 1) | i <- [1..size] ]

printElement :: IntArray -> (Int, Int) -> IO ()
printElement arr (i,j) = do
  putChar $ if arr ! (i,j) == 0
            then '0'
            else '1'
  if j == size
    then putChar '\n'
    else return ()

main = do
  let arr2 :: IntArray
      arr2 = updateArray arr
  -- 4. Print the array
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ printElement arr2 (i, j)
{% endhighlight %}

Recall that `(//)` takes a list of updates, so we can calculate all the places that we need to update and pass them in all at once. `arr` is the original zero matrix, while `arr2` is the updated identity matrix. We make sure that `printElement` takes the array as a parameter, and call it with `arr2`.

In subsequent sections, we'll mostly show various ways to implement `updateArray` using mutation-like constructs. You can assume the rest of the code remains the same.

## Shadowing

In Haskell, later variables with the same name "shadow" earlier variables, so we can pretend that we've updated the array in-place by giving the new array the same name:

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr =
  case arr // [((1,1), 1)] of
    arr -> case arr // [((2,2), 1)] of
      arr -> case arr // [((3,3), 1)] of
        arr -> case arr // [((4,4), 1)] of
          arr -> case arr // [((5,5), 1)] of
            arr -> case arr // [((6,6), 1)] of
              arr -> case arr // [((7,7), 1)] of
                arr -> case arr // [((8,8), 1)] of
                  arr -> case arr // [((9,9), 1)] of
                    arr -> case arr // [((10,10), 1)] of
                      arr -> arr
{% endhighlight %}

`updateArray` in the above code is supposed to be similar to this Perl:
{% highlight perl %}
# 1. Declare the array
my $array;
# 3. Modify the array
$array->[0]->[0] = 1;
$array->[1]->[1] = 1;
$array->[2]->[2] = 1;
$array->[3]->[3] = 1;
$array->[4]->[4] = 1;
$array->[5]->[5] = 1;
$array->[6]->[6] = 1;
$array->[7]->[7] = 1;
$array->[8]->[8] = 1;
$array->[9]->[9] = 1;
{% endhighlight %}

Another way of writing it in Haskell that avoids the deep nesting is:

{% highlight haskell %}
import Control.Monad.Identity

updateArray :: IntArray -> IntArray
updateArray arr = runIdentity $ do
  arr <- return $ arr // [((1,1), 1)]
  arr <- return $ arr // [((2,2), 1)]
  arr <- return $ arr // [((3,3), 1)]
  arr <- return $ arr // [((4,4), 1)]
  arr <- return $ arr // [((5,5), 1)]
  arr <- return $ arr // [((6,6), 1)]
  arr <- return $ arr // [((7,7), 1)]
  arr <- return $ arr // [((8,8), 1)]
  arr <- return $ arr // [((9,9), 1)]
  arr <- return $ arr // [((10,10), 1)]
  return arr
{% endhighlight %}

or

{% highlight haskell %}
setDiagonal :: Int -> IntArray -> IntArray
setDiagonal i arr = arr // [((i,i), 1)]

updateArray :: IntArray -> IntArray
updateArray arr = runIdentity $ do
  arr <- return $ setDiagonal 1 arr
  arr <- return $ setDiagonal 2 arr
  arr <- return $ setDiagonal 3 arr
  arr <- return $ setDiagonal 4 arr
  arr <- return $ setDiagonal 5 arr
  arr <- return $ setDiagonal 6 arr
  arr <- return $ setDiagonal 7 arr
  arr <- return $ setDiagonal 8 arr
  arr <- return $ setDiagonal 9 arr
  arr <- return $ setDiagonal 10 arr
  return arr
{% endhighlight %}

The `do` notation above is in `Identity` context, which is a way of chaining together ordinary Haskell values and functions using the `do` syntax. The compiler should convert it to deeply-nested case statements, or something equivalent.

I needed to use `do` notation or `case` because pattern matches are non-recursive. This will produce an infinite loop, because `let` bindings are recursive by default; so `arr` will be defined in terms of itself, not the previous array.

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr =
  let arr = arr // [((1,1), 1)]
  in arr
{% endhighlight %}

There are two shortcomings with using shadowing as a replacement for mutation:

* It's a little verbose to repeat the variable name twice when "reading" and "writing".
* It works when there are a fixed number of updates(`10`), but not in e.g. a while loop that repeatedly updates a variable.

## Function Composition

Look back at this example from __Shadowing__:

{% highlight haskell %}
import Control.Monad.Identity

updateArray :: IntArray -> IntArray
updateArray arr = runIdentity $ do
  arr <- return $ setDiagonal 1 arr 
  arr <- return $ setDiagonal 2 arr 
  arr <- return $ setDiagonal 3 arr 
  arr <- return $ setDiagonal 4 arr 
  arr <- return $ setDiagonal 5 arr 
  arr <- return $ setDiagonal 6 arr 
  arr <- return $ setDiagonal 7 arr 
  arr <- return $ setDiagonal 8 arr 
  arr <- return $ setDiagonal 9 arr 
  arr <- return $ setDiagonal 10 arr
  return arr
{% endhighlight %}

Since we don't really do anything with the variable other than feed it into the next binding, some Haskellers would remove the variables and compose the functions together:

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray =
  setDiagonal 10 .
  setDiagonal 9 .
  setDiagonal 8 .
  setDiagonal 7 .
  setDiagonal 6 .
  setDiagonal 5 .
  setDiagonal 4 .
  setDiagonal 3 .
  setDiagonal 2 .
  setDiagonal 1
{% endhighlight %}

Or

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr =
  setDiagonal 10 $
  setDiagonal 9 $
  setDiagonal 8 $
  setDiagonal 7 $
  setDiagonal 6 $
  setDiagonal 5 $
  setDiagonal 4 $
  setDiagonal 3 $
  setDiagonal 2 $
  setDiagonal 1 $
  arr
{% endhighlight %}

Or

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = foldr ($) arr [ setDiagonal i | i <- [1..10] ]
{% endhighlight %}

Or(from [`Data.Monoid`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Monoid.html#t:Endo)):

{% highlight haskell %}
import Data.Monoid

composeAll :: [a -> a] -> (a -> a)
composeAll fs = appEndo $ mconcat $ map Endo fs

updateArray :: IntArray -> IntArray
updateArray = composeAll [ setDiagonal i | i <- [1..10] ]
{% endhighlight %}

This works okay when we have a chain updating 1 variable. You can extend it to more by using a tuple:

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr =
  getArray $
  updateNext $ -- (1,1)
  updateNext $ -- (2,2)
  updateNext $ -- (3,3)
  updateNext $ -- (4,4)
  updateNext $ -- (5,5)
  updateNext $ -- (6,6)
  updateNext $ -- (7,7)
  updateNext $ -- (8,8)
  updateNext $ -- (9,9)
  updateNext $ -- (10,10)
  (arr, 1, 1)
  where
    updateNext :: (IntArray, Int, Int) -> (IntArray, Int, Int)
    updateNext = \(arr, i, j) -> (arr // [((i,j), 1)], i+1, j+1)
    
    getArray :: (IntArray, Int, Int) -> IntArray
    getArray (arr, _, _) = arr
{% endhighlight %}

Here, we make the indices `i` and `j` part of the state, and increment them after each application of `updateNext`. At the end, we would get the new array, `i`, and `j`; but we only care about the array, so `getArray` discards `i` and `j`.

One shortcoming of this technique is that you have to ensure that the state parameter is always last, to compose the functions. See the `State` monad for a way of hiding the state paremeter.

## Tail-recursion

The most powerful way to write high-performance Haskell is to make your state explicit, and call an inner function written in tail-recursive style. I'll add the variables to the previous example:

{% highlight haskell %}
type LoopState = (Int, Int)

updateArray :: IntArray -> IntArray
updateArray arr =
  let (st1, arr1) = updateNext (initialState, arr)
      (st2, arr2) = updateNext (st1, arr1)
      (st3, arr3) = updateNext (st2, arr2)
      (st4, arr4) = updateNext (st3, arr3)
      (st5, arr5) = updateNext (st4, arr4)
      (st6, arr6) = updateNext (st5, arr5)
      (st7, arr7) = updateNext (st6, arr6)
      (st8, arr8) = updateNext (st7, arr7)
      (st9, arr9) = updateNext (st8, arr8)
      (st10, arr10) = updateNext (st9, arr9)
  in arr10
  where
    initialState :: LoopState
    initialState = (1,1)
    
    updateNext :: (LoopState, IntArray) -> (LoopState, IntArray)
    updateNext = \((i, j), arr) -> ((i+1, j+1), arr // [((i,j), 1)])
{% endhighlight %}

One problem with this is that we're duplicating the same statement 10 times. It's a little silly to have 18 lines when the Perl is only 3:

{% highlight perl %}
for (my $idx = 0; $idx < $size; $idx++) {
    $array->[$idx]->[$idx] = 1;
}
{% endhighlight %}

The `for` loop consists of 4 components:
* An initial state: `my $idx = 0`, and `$array`
* A condition that reads the state: `$idx < $size`
* Statements advancing the state: `$idx++`, and `$array->[$idx]->[$idx] = 1;`
* (Optional) A way to convert the state into a return value: `$array` after the loop.

Here is the most general way to convert a loop into a tail-recursive `loop` function:
{% highlight haskell %}
type LoopState = Int

updateArray :: IntArray -> IntArray
updateArray arr = loop initialState arr
  where
    initialState :: LoopState
    initialState = 1

    loop :: LoopState -> IntArray -> IntArray
    loop i arr =
      if shouldLoop
      then loop nextState nextArr
      else finalize i arr
      where
        shouldLoop = i <= 10
        nextState = i+1
        nextArr = setDiagonal i arr

    finalize :: LoopState -> IntArray -> IntArray
    finalize i arr = arr
{% endhighlight %}

With this, it's mechanical to replace any simple for loops with a tail-recursive function. The only loops that can't be converted are those that do something other than pure computation, such as write to a logfile or connect to a database. For those, see __Recursive IO__ below.

Here's a shorter function that I would be more likely to use in real code:
{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = loop 1 arr
  where
    loop i arr = if i <= 10
                 then loop (i+1) (setDiagonal i arr)
                 else arr
{% endhighlight %}

## State Monad

In the __Function Composition__ section, we saw that we can emulate mutable variables by making the state the last parameter in a sequence of functions and composing them.

The [`State`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Strict.html) monad is a way of automatically threading this state parameter to all code that uses it. I'll add the variables back to the previous example:

{% highlight haskell %}
type LoopState = Int

updateArray :: IntArray -> IntArray
updateArray arr =
  let (st1, arr1) = updateNext (initialState, arr)
      (st2, arr2) = updateNext (st1, arr1)
      (st3, arr3) = updateNext (st2, arr2)
      (st4, arr4) = updateNext (st3, arr3)
      (st5, arr5) = updateNext (st4, arr4)
      (st6, arr6) = updateNext (st5, arr5)
      (st7, arr7) = updateNext (st6, arr6)
      (st8, arr8) = updateNext (st7, arr7)
      (st9, arr9) = updateNext (st8, arr8)
      (st10, arr10) = updateNext (st9, arr9)
  in arr10
  where
    initialState :: LoopState
    initialState = 1
    
    updateNext :: (LoopState, IntArray) -> (LoopState, IntArray)
    updateNext = \(i, arr) -> (i+1, setDiagonal i arr)
{% endhighlight %}

A value of type `State s a` is a value of type `a` that implicitly reads and writes a state parameter of type `s`. You almost always want the strict version. Here is the above code written to use `State Int IntArray`:

{% highlight haskell %}
import Control.Monad.State.Strict
type LoopState = (Int, IntArray)

updateArray :: IntArray -> IntArray
updateArray arr = evalState action initialState
  where
    action :: State LoopState IntArray
    action = do
      updateNext -- 1
      updateNext -- 2
      updateNext -- 3
      updateNext -- 4
      updateNext -- 5
      updateNext -- 6
      updateNext -- 7
      updateNext -- 8
      updateNext -- 9
      updateNext -- 10
      
    initialState :: LoopState
    initialState = (1, arr)
    
    updateNext :: State LoopState IntArray
    updateNext = do
      (i, arr) <- get
      let arr' = setDiagonal i arr
      put (i+1, arr')
      return arr'
{% endhighlight %}

The pattern match `(i, arr) <- get` pattern matches the state that is implicitly being passed around. `put (i+1, arr')` passes a new state to the next statement.

Take a moment to see how the tuple is being passed along. It's entirely equivalent to the `let` binding example.

This can also be written much shorter:

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = evalState (replicateM 10 updateNext) (1, arr)
  where
    updateNext :: State LoopState IntArray
    updateNext = state $ \(i, arr) -> (arr', (i+1, arr'))
      where arr' = setDiagonal i arr
{% endhighlight %}

You can also mix tail recursive style to emulate loops that change mutable variables:

{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = evalState loop (1, arr)
  where
    loop :: State LoopState IntArray
    loop = do
      (i, arr) <- get
      if i <= 10
      then do
        put (i+1, setDiagonal i arr)
        loop
      else return arr
{% endhighlight %}

Or the shorter:
{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = evalState loop (1, arr)
  where
    loop :: State LoopState IntArray
    loop = do
      (i, arr) <- get
      if i <= 10
      then do
        put (i+1, setDiagonal i arr)
        loop
      else return arr
{% endhighlight %}

## ST Monad

The `IORef` example had the disadvantage that it required code to be in `IO` context to read or write references. Code in `ST` context also allows mutable variables, and can be embedded anywhere. Instead of an `IORef`, you'll want an [`STRef`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-STRef.html).

{% highlight haskell %}
import Control.Monad.ST
import Data.STRef

updateArray :: IntArray -> IntArray
updateArray arr = runST $ do
  iRef <- newSTRef 1
  arrRef <- newSTRef arr
  let loop = do
        i <- readSTRef iRef
        arr <- readSTRef arrRef
        if i <= 10
          then do
            writeSTRef iRef (i+1)
            writeSTRef arrRef (setDiagonal i arr)
            loop
          else return arr
  loop
{% endhighlight %}

## Implicit Parameters

In __Shadowing__, we learned that you can sometimes simulate mutation by creating a new value with the same name, rather than updating the existing value. In __State Monad__, we learned that you can make this more composable by hiding the parameters and implicitly threading them along. `ImplicitParams` is a language extension that allows you to thread hidden parameters anywhere in your code.

Their chief advantage is that you can add them to any existing implementation code and it will still compile. You may need to update type signatures, but in well-structured code you'll often only have a single program-specific type alias to change.

I've used them for global configuration before: Imagine connecting to a database on program startup to retrieve configuration information, which is accessible throughout most of the program. You usually wouldn't use them for inner loops or computation, so I've changed the example for this section.

Suppose we've unwisely chosen a `List` as the data structure for our program, and it uses too many `List`-specific utility functions to easily change to a different data type. Eventually, we plan to switch to an array or [`Data.Vector`](https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html), but in the meantime there are performance issues because we use `length` everywhere(which is $$O(n)$$ for linked lists). You can use Implicit Parameters to calculate the length of a list once, and pass it around.

Here's the current code:
{% highlight haskell %}
import Control.Monad.State

type ProgramMonad a = State [Double] a

-- We want engagement to be higher for customers with fewer sales
-- so they buy the 10+ package.
importantCalculation :: ProgramMonad Double
importantCalculation = do
  arr <- get
  if length arr > 10
  then return $ sum $ map log arr
  else return $ product $ map exp arr

main = putStrLn $ show $ evalState importantCalculation [5,8,9,3,3,1]
{% endhighlight %}

The `length arr > 10` is slowing down the calculation. Add the implicit parameter:
{% highlight haskell %}
type ProgramMonad a = (?length :: Int) => State [Double] a

main =
  let ?length = length arr
  in putStrLn $ show $ evalState importantCalculation arr
  where
    arr = [5,8,9,3,3,1]
{% endhighlight %}

The important thing above is that we added the implicit parameter without needing to change any of the code in the calculation: We only changed the type signature, and set `?length` once near the beginning of our program. Of course, now that it's available, we can temporarily fix the performance problem by using it:

{% highlight haskell %}
importantCalculation :: ProgramMonad Double
importantCalculation = do
  arr <- get
  if ?length > 10
  then return $ sum $ map log arr
  else return $ product $ map exp arr
{% endhighlight %}

## Recursive IO

You can mix recursive style with `IORef` to write code that feels like C in Haskell. Here's the plain version:

{% highlight haskell %}
import Control.Monad
import Data.Array
import Data.IORef

type IntArray = Array (Int,Int) Integer

setDiagonal :: Int -> IntArray -> IntArray
setDiagonal i arr = arr // [((i,i), 1)]

updateArray :: IORef IntArray -> IO ()
updateArray arrRef = do
  iRef <- newIORef 1
  let loop = do
        i <- readIORef iRef
        arr <- readIORef arrRef
        if i <= 10
          then do
          writeIORef iRef (i+1)
          writeIORef arrRef (setDiagonal i arr)
          loop
          else return ()
  loop

size = 10

printElement :: IORef IntArray -> (Int, Int) -> IO ()
printElement ref (i,j) = do
  arr <- readIORef ref
  putChar $ if arr ! (i,j) == 0
            then '0'
            else '1'
  if j == size
    then putChar '\n'
    else return ()

main :: IO ()
main = do
  -- 1. Declare the array
  arrRef <- newIORef $ array ((1,1), (size,size)) $ do
    -- 2. Initialize the array to 0
    i <- [1..size]
    j <- [1..size]
    return ((i,j), 0)

  -- 3. Set the diagonal to 1
  updateArray arrRef

  -- 4. Print the array
  sequence_ $ do
    i <- [1..size]
    j <- [1..size]
    return $ printElement arrRef (i,j)
{% endhighlight %}

And here's a few variations with some helper functions:

Use `when` instead of having an empty `else` clause:
{% highlight haskell %}
updateArray :: IORef IntArray -> IO ()
updateArray arrRef = do
  iRef <- newIORef 1
  let loop = do
        i <- readIORef iRef
        arr <- readIORef arrRef
        when (i <= 10) $ do
          writeIORef iRef (i+1)
          writeIORef arrRef (setDiagonal i arr)
          loop
  loop
{% endhighlight %}

Use `replicateM_` to repeat the action rather than hand-coding a recursive loop:
{% highlight haskell %}
updateArray :: IORef IntArray -> IO ()
updateArray arrRef = do
  iRef <- newIORef 1
  replicateM_ 10 $ do
    i <- readIORef iRef
    arr <- readIORef arrRef
    writeIORef iRef (i+1)
    writeIORef arrRef (setDiagonal i arr)
{% endhighlight %}

Use `modifyIORef'` rather than reading and writing:
{% highlight haskell %}
updateArray :: IORef IntArray -> IO ()
updateArray arrRef = do
  iRef <- newIORef 1
  replicateM_ 10 $ do
    i <- readIORef iRef
    writeIORef iRef (i+1)
    modifyIORef' arrRef (setDiagonal i)
{% endhighlight %}

All-in-all, it's not the most elegant, but it gets the job done.

## Conclusion

You've seen a lot of different ways to do the same thing. Hopefully some of them are new, and help you port your own imperative thinking and code over to Haskell. Send me a message on Twitter if this helped at all.

The best one is probably this:
{% highlight haskell %}
updateArray :: IntArray -> IntArray
updateArray arr = arr // [((i,i), 1) | i <- [1..size] ]
{% endhighlight %}