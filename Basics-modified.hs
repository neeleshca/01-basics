{-
---
fulltitle: Haskell Basics
date: September 7, 2022
---

Welcome to Haskell!
-------------------

If you are reading this text online, you may wish to access it as a Haskell
project instead. You can find this module as part of the repository
[01-basics](https://github.com/upenn-cis5520/01-basics) on github. We have prepared
instructions on using
[github with CIS 5520](https://www.cis.upenn.edu/~cis5520/current/version.html)
and with installing
[GHC and VSCode](https://www.cis.upenn.edu/~cis5520/current/haskell-vscode.html).
We strongly encourage you to read this file in the VSCode editor so that you
can experiment with it.

Every Haskell file begins with a few lines naming the module (this name must
start with a capital letter and be the same as the file name) and (optionally)
importing definitions from other modules.
-}

module Basics where

-- library imports must come at the beginning

import Control.Concurrent (yield)
import Data.Bits (Bits (xor))
import Test.HUnit
  ( Counts (Counts),
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )
import Prelude hiding (const, sum, take)

{-
Observe that Haskell supports two kinds of comments: single line comments
start with `--` and block comments begin and end with `{-` and `-}`
respectively.

Understanding a Haskell program is about equality
-------------------------------------------------

*Functional* programming means that the semantics of a program can be
described mathematically. One principle of mathematics is called Leibniz
equality: in any context, we can replace an object with anything equivalent
to it.  Therefore, in Haskell, we reason about computation by reasoning about
*equality* of (sub-)expressions.

For example, if we want to know what value an arithmetic expression computes
to, we only need to find some number that is equal to it.

    3 * (4 + 5)

    { 4+5 is equal to 9 by addition, so we can replace it in the expression }

    3 * 9

    { by multiplication }

    27

That's it!

Furthermore, we can ask VSCode to compute the value of an expression for us with a
special form of comment (i.e. a single line comment that starts with '>>>').

Try clicking on "Evaluate..." below.
-}

-- >>> 3 * (4 + 5)
-- 27

{-
A Haskell module (like this one) is a list of *definitions*. These definitions
allow us to give names to Haskell expressions.
-}

ex :: Integer
ex = 3 * (4 + 5)

{-
We can ask VSCode to calculate these values, just as we did above.
-}

-- >>> ex
-- 27

{-
Whenever we give a name to an expression, it is a good idea to also write down
its type. Although Haskell can often figure out this type for you, when you
are just starting out, the error messages can be perplexing without these
additional hints.

The `Integer` type is the type of arbitrarily large integers in Haskell.
-}

bigInteger :: Integer
bigInteger = 12345678901234567890

{-
This is in contrast to the `Int` type, for word-sized integers (machine
dependent). Numbers are overloaded in Haskell, so the type annotation tells
the compiler how to interpret this expression. (And, note the warning
issued by the compiler for this out of range number!)
-}

bigInt :: Int
bigInt = 12345678901234567890

{-
Compare the value of a extra-large `Integer`
-}

-- >>> bigInteger
-- 12345678901234567890

{-
with an `Int`
-}

-- >>> bigInt
-- -6101065172474983726

{-
Above, we declared the type of an expression separately from giving it a
name. However, if we don't want to give a name to an expression, we can
still annotate it with its type using `::`.
-}

-- >>> 31 * (42 + 56) :: Integer
-- 3038

{-
More generally, the type annotation can be attached to any subexpression, not
just at the top level.
-}

-- >>> (31 :: Integer) * (42 + 56)
-- 3038

{-
It is good style to annotate the type of *every* declaration in a Haskell
program. This helps with error messages, as Haskell operators, like `*`, and
constants like '31', are often overloaded.

Elements of Haskell
-------------------

So far, we have have seen the following three properties of Haskell:

* Haskell code is based on *expressions*
* Expressions evaluate to *values*
* Every expression has a *type*, which may influence evaluation

You are probably familiar with expressions in other programming languages,
where they are often used to compute numeric and boolean values. Haskell also
includes these types and operators.

For example, Haskell includes floating point numbers, via the `Double` type,
using the same overloaded syntax.
-}

-- >>> 31 * (42 + 56) :: Double    -- double precision floating point
-- 3038.0

{-
Furthermore, you'll also find characters, strings and boolean values.
-}

-- >>> 'a' :: Char                 -- characters
-- 'a'

-- >>> "abcd" :: String            -- strings
-- "abcd"

-- >>> "cis" ++ "552"              -- string concatenation
-- "cis552"

-- >>> True :: Bool                -- boolean values
-- True

-- >>> 1 <= 3 || False && 3 > 2    -- boolean operators, comparisons
-- True

{-
What is a little different about Haskell is that everything is an expression,
including conditionals. This means that `if` can be nested inside other
expressions.
-}

-- >>> ex
-- 27

-- >>> (if ex > 28 then 1 else 0) + 2 :: Int
-- 2

{-
Now the last basic type, shown below, is subtle. It is a special constant,
written `()` and called "unit". The type of this constant is written with the
same notation, and also called "unit". You'll need to pay attention to
context to know whether we mean the constant or the type. The key fact about
this basic type is that there is only *one* value with type `()`.
-}

-- >>> () :: ()            -- 'unit' (both value and type have the same syntax)
-- ()

{-
What is Abstraction?
--------------------

We're going to talk a lot about *abstraction* in this course, starting from
simple examples and getting dramatically more expressive. But what is
abstraction, exactly?

The first answer is: "Pattern Recognition"

    31 * (42 + 56)

    70 * (12 + 95)

    90 * (68 + 12)

What do these expressions have in common? They all follow the same general
pattern. We can generalize that pattern to a *function* by defining an equation.
-}

pat :: Integer -> Integer -> Integer -> Integer
pat a b c = a * (b + c)

{-
We call functions by providing them with arguments.
-}

-- >>> pat 31 42 56
-- 3038

{-
No parentheses are necessary, unless the argument itself is a compound expression.
-}

-- >>> pat (30 + 1) 42 56
-- 3038

{-
The important question is not "What does this function do?"
but, instead "What does this function mean?" We can reason
about that meaning using what we know about equality.

    pat 31 42 56

    { function call, replace a b & c in right-hand side of equation by 31 42 and 56 }

    == 31 * (42 + 56)

    { addition }

    == 31 * 98

    { multiplication }

    == 3038

Functions, like `pat`, are the core abstraction mechanisms in functional
programming.

Function types
--------------

Like all expressions, functions have types.

The type of a function taking an input of type `A` and yielding an output of
type `B` is written as

    A -> B

For example, the `pos` function determines whether an `Int` is strictly
greater than zero.
-}

pos :: Int -> Bool
pos x = x > 0

{-
The `pat` function above takes multiple arguments. Therefore, its type has
multiple `->`s, one for each argument.

The type of a function taking inputs of type `A1`, `A2`, and `A3` and
returning a result of type `B` is written as

    A1 -> A2 -> A3 -> B

Symbolic vs. alphabetic names
-----------------------------

Symbolic identifiers (i.e. `+` and `*`) are infix by default.

Parentheses around a symbolic name turn it into a regular
name.

For example, if we want to define an alphabetic name for the
addition function, we can do so.
-}

plus :: Int -> Int -> Int
plus = (+)

-- >>> plus 5 7
-- 12

-- >>> 5 `plus` 7
-- 12

{-
And we can call operations in parentheses just like "standard" functions, by
writing their arguments afterwards.
-}

p0 :: Int
p0 = (+) 2 4

{-
Likewise we can use alphabetic name in backquotes as infix.
-}

p1 :: Int
p1 = 2 `plus` 2

{-
Laziness is a virtue
--------------------

One major difference between Haskell and other programming languages is that
Haskell uses a "call-by-need" semantics for evaluation, aka "lazy" evaluation.
What this means is that Haskell does not evaluate arguments before calling
functions. Instead, expressions are only evaluated when they are needed.

We can observe this behavior in Haskell by seeing what happens when we use
`error` in a subexpresion. The `error` keyword in Haskell triggers a
non-recoverable runtime exception, aborting any computation in progress.
An `error` can be used in any context and can be given
any type because it does not produce a value.
If an error is triggered, then we know that subexpression was evaluated.

For example, addition always needs to evaluate its arguments, so this
error will trigger.
-}

-- >>> 1 + 2 + 3 + error "Here!"
-- Here!

{-
However, we won't trigger an error that is in dead code, such as in
the non-selected part of an if-expression...
-}

-- >>> if 1 < 3 then 5 else error "Unreachable"
-- 5

{-
..or that was short-circuited when evaluating a boolean expression.
-}

-- >>> True || error "Unreachable"
-- True

{-
In contrast, you can see that if the first argument were `False` instead,
it does not short circuit and does not trigger the error.
-}

-- >>> False || error "Ooops!"
-- Ooops!

{-
In most languages, `if` and `||` are defined via special constructs because they
include sub-expressions that are not always evaluated. However, in Haskell, these
constructs are less special. For example, you can define your own short-circuiting
version of the `or` operator. Suppose you would like this operator to be written
with three pipes instead of two:
-}

(|||) :: Bool -> Bool -> Bool
(|||) a b = if a then True else b

{-
Through laziness, this definition short circuits, just like the Prelude version of `||`.
-}

-- >>> True ||| error "Unreachable"
-- True

{-
More generally, because Haskell is lazy, the language enables more abstraction.
Functions and operators that we define can have nontrivial control behavior.

Laziness is also the reason that we can reason about Haskell programs just by thinking
about equalities. For example, there is a function in the Prelude with the following
definition:
-}

const :: a -> b -> b
const x y = y

{-
In a call-by-value language (i.e. most languages) if you see a subexpression like
`const (f 3) 4`, you have to know whether the expression `f 3` produces a normal value
first before you can know that the result is 4. However, in Haskell, you can use
substitution: the pattern above says that with any call to `const`, the result is the value of
the second argument.

Thus:
-}

-- >>> const (error "Here!") 4
-- 4

-- >>> const (6) 12313
-- 12313

{-
We'll see more examples of laziness throughout the semester. Sometimes we use the word "strictness" to
describe how functions use their arguments. If an argument will always be evaluated before the
function is called, we call this argument "strict." For example, both arguments in an addition
operation are strict, because we need to know what the numbers are to sum them together. However,
only the first argument in `||` is strict because it does not evaluate the second argument when
the value of the first one is `True`.

Making Haskell DO something
===========================

Programs often interact with the world:

* Read files
* Display graphics
* Broadcast packets
* Run test cases and print success or failure

They don't *just* compute values.

How does this fit with values & equalities above?

Note, we've gotten far without doing any I/O. That's fairly standard in
Haskell. Working with VSCode means that we can see the answers directly, we
don't need an action to print them out. However, a standalone executable needs
to do *something*, so we demonstrate that next.

The GHC System
--------------

We'll start with a few examples just using the interactive toplevel for the
Haskell language. Although Haskell is a compiled language, the interactive
toplevel, "ghci" is available for experimentation. You can access this
toplevel using any command prompt (i.e. Terminal), as long as you have GHC
installed. The examples below also assume that you have the "stack" tool
available and that you have started the command prompt in the same directory
that contains this source code. [Instructions for installing "stack" and
other tools are available.](https://www.cis.upenn.edu/~cis5520/current/haskell-vscode.html)

First use the terminal to start `ghci` and instruct it to load the `Basics`
module.

    sweirich@sixteen 01-basics % stack ghci Basics.hs
    Using configuration for cis5520-basics:lib to load /Users/sweirich/552/cis5520-20fa/lectures/01-basics/Basics.lhs
    cis5520-basics> configure (lib)
    Configuring cis5520-basics-0.1.0.0...
    cis5520-basics> initial-build-steps (lib)
    Configuring GHCi with the following packages: cis5520-basics
    GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling Basics           ( /Users/sweirich/5520/cis5520-22fa/lectures/01-basics/Basics.lhs, interpreted )
    Ok, one module loaded.
    Loaded GHCi configuration from /private/var/folders/p3/pkythxvx6rq9q054797y4bb80000gn/T/haskell-stack-ghci/48e82592/ghci-script
    *Basics>

Now, with the prompt from the module, you can ask for the values, types and
information of expressions.

    *Basics> ex
    27
    *Basics> :t ex
    ex :: Integer
    *Basics> :i ex
    ex :: Integer     -- Defined at Basics.hs:5:1

Obligatory Hello World
----------------------

"IO actions" are a new sort of sort of value that describe an effect
on the world.

    IO a  --  Type of an action that returns an `a`  (a can be anything!)

Actions that *do* something but return nothing have the type `IO ()`.

    putStr :: String -> IO ()

So `putStr` takes in a string and returns an *action* that writes the string
to stdout.

Because `putStr` doesn't evaluate to a value that we can print, we can't play with
it using the IDE. Even if you hit `Evaluate...` you will see nothing because the
IDE hides the printing action and only displays the value.
-}

-- >>> putStr "Say what?"

{-
Instead, to observe the printing action, we need to use GHCi. Let's give it
name first:
-}

hw :: IO ()
hw = putStr "Hello World!\n"

{-
and then we can give it a try.

    *Basics> hw
    Hello World!
    *Basics>

Just 'do' it
-------------
How can we do many actions?  By composing small actions.

The `do` syntax allows us to create a compound action that sequences one
action after another. The definition of `many` below is a compound action
that outputs the three strings in order. (Try it out in ghci!)
-}

many :: IO ()
many = do
  putStr "Hello" -- each line in the sequence
  putStr " World!" -- must be an IO action
  putStr "\n" -- don't forget the newline

{-
Note: white-space is significant here. The `do` notation sequences actions, but
each action in the sequence must start at the same character offset: all of
the `putStr`s must be lined up.

Sometimes people put the `do` on a line by itself and then start the list
of actions on the next line. This saves column width in larger developments.
-}

many' :: IO ()
many' = do
  putStr "Hello"
  putStr " World!"
  putStr "\n"

{-
Example: Input Action
---------------------

Actions can also return a value.

    getLine :: IO String

This action reads and returns a line from stdin.  We can name the result
as part of a `do` sequence, with this notation

    x <- action

Here `x` is a variable that can be used to refer to the result of the
action in later code.
-}

query :: IO ()
query = do
  putStr "What is your name? "
  n <- getLine
  let y :: String
      y = "Welcome to CIS 552 " ++ n
  putStrLn y

{-
When we sequence actions of type `IO ()` there is no need to name the
result. These actions do not return anything interesting. We could name the
result if we wanted (such as `_m` below); but because of its type we know
that `_m` will always be a special value, written `()` and called "unit". By
convention, the name of an unused variable starts with an underscore.
-}

query' :: IO ()
query' = do
  _m <- putStr "What is your name? "
  n <- getLine
  putStrLn ("Welcome to CIS 552 " ++ n)
  _st <- query2
  return ()

{-
Note that you cannot name the *last* action in a sequence. Names are there so that
you can use their results later. If you want to return the value instead, the last action
should be a `return`.
-}

query2 :: IO String -- compare this type to `query` above.
query2 = do
  putStr "What is your name? "
  n <- getLine
  return n

{-
Example: Testing Actions
------------------------

The `hunit` library contains definitions for constructing unit tests for your
programs. You must import this library at the top of your module (with
import Test.HUnit`) before you can access these definitions.  This library
defines a `Test` type for test cases.
-}

t1 :: Test
t1 = (1 + 2 :: Int) ~?= 3

{-

* Haskell is lazy, so these definitions *create* tests, but don't actually run

them yet. We'll do that below.

* The `(~?=)` operator is overloaded. You can create tests that compare

expressions at many different types. When the expressions themselves are also
overloaded (such as those with numbers), we run into ambiguity---what type of
expressions should this test actually use? We resolve that ambiguity with
an internal typing annotation `(3 :: Int)`.

To run the test case, we use the function `runTestTT`.

           runTestTT :: Test -> IO Counts
-}

numTest :: IO Counts
numTest = runTestTT t1

-- >>> numTest
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

tup1 :: (Char, Int)
tup1 = ('5', 6)

tpat :: (Int, Int, Int) -> Int
tpat (a, b, c) = a * (b + c)

tup4 :: ((Int, Int), Int)
tup4 = ((2, 3), 4)

tup5 :: (Int, (Int, Int))
tup5 = (2, (3, 4))

tup6 :: (Int, Int, Int)
tup6 = (2, 3, 4)

pat4 :: ((Int, Int), Int) -> Int
pat4 ((a, b), c) = a + (b * c)

pat5 :: (Int, (Int, Int)) -> Int
pat5 (a, (b, c)) = a + (b * c)

pat6 :: (Int, Int, Int) -> Int
pat6 (a, b, c) = a + (b * c)

-- >>> pat4 tup4
-- 14

-- >>> pat5 tup5
-- 14

-- >>> pat6 tup6
-- 14

-- >>> tup1
-- ('5',6)

act2 :: (IO (), IO ())
act2 = (putStr "Hello1\n", putStr "Hello2\n")

runAct2 :: IO ()
runAct2 = do
  let (x, y) = act2
  x
  y

runAct2' :: IO ()
runAct2' = do
  let (x, y) = act2
  y
  x

runAct2'' :: IO ()
runAct2'' = do
  let (x, y) = act2
  x
  x

m1 :: Maybe Int
m1 = Just 4

m2 :: Maybe Int
m2 = Nothing

-- >>> m1
-- Just 2

pat'' :: Maybe Int -> Int
pat'' (Just x) = x
pat'' Nothing = 2

-- >>> pat'' m2
-- 2
-- >>> pat'' (Just 4)
-- 4

jn :: Maybe (Maybe a) -> Maybe a
jn (Just (Just x)) = Just x
jn (Just Nothing) = Nothing
jn Nothing = Nothing

-- >>> jn (Nothing)
-- Nothing

location :: String -> Maybe String
location "test1" = Just "test_location1"
location "test2" = Just "test_location2"
location _ = Nothing

-- >>> location "test3"
-- Nothing

l1 :: [Double]
l1 = [1.1, 1.2, 1.3]

l2 :: [Int]
l2 = [1, 2, 3]

-- >>> l1
-- [1.1,1.2,1.3]

-- >>> l2
-- [1,2,3]

l3 :: [(Int, Int)]
l3 = [(1, 2)]

-- >>> l3
-- [(1,2)]

l4 :: [[Int]]
l4 = [[1, 2], [3, 4]]

-- >>> l4
-- [[1,2],[3,4]]

-- l5 :: [Int]
-- l5 = [1, True]

l6 :: [a]
l6 = []

l7 :: [Char]
l7 = ['h', 'e', 'l', 'l', 'o', '1']

-- >>> l7
-- "hello1"

c1Tmp :: [Bool]
c1Tmp = [False, False, True]

c1 :: [Bool]
c1 = True : c1Tmp

c2 :: [Bool]
c2 = True : []

-- >>> c1Tmp
-- >>> c1
-- >>> c1Tmp
-- [False,False,True]
-- [True,False,False,True]
-- [False,False,True]

-- [False,False]
-- [True,False,False]

-- >>> c2
-- [True]

c3 :: [[a]]
c3 = [] : []

-- >>> c3
-- [[]]

-- >>> [1, 2, 3, 4] == 1 : 2 : 3 : 4 : []
-- True

testClone1, testClone2, testClone3, testClone4 :: Test
testClone1 = clone 'a' 4 ~?= ['a', 'a', 'a', 'a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]
testClone4 = clone 'a' (-1) ~?= []

clone :: a -> Int -> [a]
clone x n = if n >= 0 then x : clone x (n - 1) else []

cl1, cl2, cl3, cl4 :: IO Counts
cl1 = runTestTT testClone1
cl2 = runTestTT testClone2
cl3 = runTestTT testClone3
cl4 = runTestTT testClone4

-- >>> cl1
-- Counts {cases = 1, tried = 1, errors = 0, failures = 1}

cls :: IO Counts
cls = runTestTT (TestList [testClone1, testClone2, testClone3, testClone4])

-- >>> cls
-- Counts {cases = 4, tried = 4, errors = 0, failures = 3}

testRange :: Test
testRange =
  TestList
    [ range 1 5 ~?= [1, 2, 3, 4, 5],
      range 1 1 ~?= [1],
      range 1 0 ~?= []
    ]

range :: Int -> Int -> [Int]
range x y = if y >= x then x : range (x + 1) y else []

-- >>> range 1 1
-- [1]

-- >>> [1] : []
-- [[1]]
-- >>> 1 : 2 : 3 : []
-- [1,2,3]

runRTests :: IO Counts
runRTests = runTestTT testRange

-- >>> runRTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

isHi :: String -> Bool
isHi "Hi" = True
isHi _ = False

-- >>> isHi "Hi"
-- True

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

-- >>> isSingleton [1, 2]
-- False

isLong :: [a] -> Bool
isLong [] = False
isLong [_] = False
isLong [_, _] = False
isLong _ = True

-- >>> isLong [2::Int]
-- False

testIsLong :: Test
testIsLong =
  TestList
    [ not (isLong []) ~? "nil", -- can convert booleans to tests by naming them via `~?`
      not (isLong "a") ~? "one",
      not (isLong "ab") ~? "two",
      isLong "abc" ~? "three"
    ]

runR1Tests :: IO Counts
runR1Tests = runTestTT testIsLong

-- >>> runR1Tests
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

isGreeting2 :: String -> Bool
isGreeting2 s =
  case s of
    "Hi" -> True
    "Hello" -> True
    _ -> False

isGreeting3 :: String -> Bool
isGreeting3 s =
  case s of
    ('H' : r) -> case r of
      "i" -> True
      "ello" -> True
      _ -> False
    _ -> False

-- >>> isGreeting3 "Hi3"
-- False

sumTests :: Test
sumTests =
  TestList
    [ sum [1, 2, 3] ~?= 6,
      sum [] ~?= 0
    ]

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

runSumTests :: IO Counts
runSumTests = runTestTT sumTests

-- >>> runSumTests
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

takeTests :: Test
takeTests =
  TestList
    [ take 1 [1, 2, 3, 4] ~?= [1],
      take 3 [1, 2, 3, 4] ~?= [1, 2, 3],
      take 5 [1, 2, 3, 4] ~?= [1, 2, 3, 4]
    ]

take :: Int -> [Int] -> [Int]
take 0 xs = []
take n [] = []
take n (x : xs) = x : take (n - 1) xs

runTakeTests :: IO Counts
runTakeTests = runTestTT takeTests

-- >>> runTakeTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

listIncTests :: Test
listIncTests =
  TestList
    [ listInc [1, 2, 3] ~?= [2, 3, 4],
      listInc [1] ~?= [2],
      listInc [] ~?= []
    ]

listInc :: [Int] -> [Int]
listInc [] = []
listInc (x : xs) = x + 1 : listInc xs

runListIncTests :: IO Counts
runListIncTests = runTestTT listIncTests

-- >>> runListIncTests
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

{-
Function practice: Double List transformation
---------------------------------------------

Define a function, called `listAdd`, that, given two lists of
numbers, adds them together pointwise. Any extra numbers are ignored.

**Step 1**: Write test cases.
-}

listAddTests :: Test
listAddTests =
  TestList
    [ listAdd [1, 2, 3] [6, 7, 8] ~?= [7, 9, 11],
      listAdd [2, 3, 4] [7, 8, 9, 10] ~?= [9, 11, 13],
      listAdd [3, 4, 5, 6] [8, 9, 10] ~?= [11, 13, 15],
      listAdd [] [] ~?= [],
      listAdd [1, 2, 3] [2, 4, 5] ~?= [3, 6, 8],
      listAdd [42] [] ~?= []
    ]

listAdd :: [Int] -> [Int] -> [Int]
listAdd [] [] = []
listAdd xs [] = []
listAdd [] ys = []
listAdd (x : xs) (y : ys) = (x + y) : listAdd xs ys

runListAddTests :: IO Counts
runListAddTests = runTestTT listAddTests

-- >>> runListAddTests
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

ones :: [Int]
ones = 1 : ones

-- >>> take 17 ones
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

allNums :: [Int]
allNums = 1 : listInc allNums

-- >>> take 3 allNums
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]

-- listInc (x : xs) = x + 1 : listInc xs
-- allNums = 1 : listInc allNums
-- allNums = 1 : listInc allNums
-- l1 = listInc allNums
--    == listInc (1 : listInc allNums)
--    == 2 : listInc (listInc allNums)
-- 1 : l1
-- 1 : 2 : listInc (listInc allNums)
-- 1 : 2 : listInc (l1)
-- 1 : 2 : listInc (2 : listInc (listInc allNums))

fibs :: [Int]
fibs = 1 : 1 : listAdd fibs (tail fibs)

-- listAdd (x : xs) (y : ys) = (x + y) : listAdd xs ys
-- 1 : 1 : listAdd fibs (tail fibs)
-- l1 = listAdd fibs (tail fibs)
-- l1 = listAdd (1 : 1 : listAdd fibs (tail fibs)) (tail (1 : 1 : listAdd fibs (tail fibs)))
-- l1 = listAdd (1 : 1 : listAdd fibs (tail fibs)) (1 : listAdd fibs (tail fibs))
-- l1 = (1+1) : listAdd (1 : listAdd fibs (tail fibs)) (listAdd fibs (tail fibs))
-- l1 = 2 : listAdd (1 : listAdd fibs (tail fibs)) (listAdd fibs (tail fibs))
-- l1 = 2 : listAdd (1 : l1) l1
-- 1 : 1 : 2 : listAdd (1 : l1) l1
-- 1 : 1 : 2 : l2
-- l2 = listAdd (1 : l1) l1
-- l2 = listAdd (1 : l1) (2 : listAdd (1 : l1) l1)
-- l2 = 3 : listAdd (l1) (listAdd (1 : l1) l1)


-- listInc (x : xs) = x + 1 : listInc xs
-- allNums = 1 : listInc allNums 
--   l1 = listInc allNums
--   l1 = listInc (1 : listInc allNums)
--   l1 = 2 : listInc (listInc allNums)
-- allNums = 1 : l1
-- allNums = 1 : 2 : listInc (listInc allNums)
-- allNums = 1 : 2 : listInc (l1)
-- allNums = 1 : 2 : listInc (2 : listInc (listInc allNums))
-- allNums = 1 : 2 : 3 : listInc (listInc (listInc allNums))