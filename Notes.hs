module Notes where
-- https://wiki.haskell.org/Keywords#where - doesn't seem to be anything important. Just some boilerplate.
-- imports, beginning

import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )
import Prelude hiding (const, sum, take)

-- >>> 3 * (4 + 5)
-- 27

ex :: Integer
ex = 3 * (4 + 5)

{-
More generally, the type annotation can be attached to any subexpression, not
just at the top level.
-}
-- Things about types. Good to annontate type for sub expressions as well.

{-
It is good style to annotate the type of *every* declaration in a Haskell
program. This helps with error messages, as Haskell operators, like `*`, and
constants like '31', are often overloaded.

So far, we have have seen the following three properties of Haskell:

Elements of Haskell
-------------------

* Haskell code is based on *expressions*
* Expressions evaluate to *values*
* Every expression has a *type*, which may influence evaluation

-}

{-
What is a little different about Haskell is that everything is an expression,
including conditionals. This means that `if` can be nested inside other
expressions.
-}


-- Maybe is pretty strong.

-- Cannot do
-- m1 :: Maybe Int
-- m1 = 2


-- Need to put in brackets to consider as single argument.
-- pat'' (Just x) = x



-- Why are we returning Maybe a?
jn :: Maybe (Maybe a) -> Maybe a
jn (Just (Just x)) = Just x
jn (Just Nothing) = Nothing
jn Nothing = Nothing
-- jn Nothing = Nothing


-- Pattern match(es) are non-exhaustive.
--  Really good feature.


-- same type


-- >>> ['a', 'b', 'c']
-- "abc"

-- >>> 'a' : ['b', 'c']
-- "abc"

-- >>> 'a' : ('b', 'c')
-- Couldn't match expected type: [Char]
--             with actual type: (Char, Char)

-- >>> 'a' : 'b' : 'c' : []
-- "abc"

-- >>> [] : 'd' : []
-- Couldn't match type ‘Char’ with ‘[a]’
-- Expected: [[a]]
--   Actual: [Char]

-- >>> [] : 'd'
-- Couldn't match expected type ‘[[a]]’ with actual type ‘Char’

-- >>> [] : [[]]
-- [[],[]]

-- >>> ['a','b']:'c'
-- Couldn't match expected type ‘[[Char]]’ with actual type ‘Char’

-- >>> 'a' : ['b', 'c']
-- "abc"

-- >>> 'a':'b':['c']
-- "abc"

-- >>>  'a' : "bc"
-- "abc"


-- >>> "b" : ["c"]
-- ["b","c"]

-- >>> "abc"
-- "abc"
