module AbstractInteger where

import           Distribution.Simple.Utils      ( xargs )

-- Here are some definations for AbstractNatural.
-- You will probably define your AbstractInteger based on
-- AbstractNatural.

data AbstractNatural = Zero | S AbstractNatural
  deriving (Show)

-- Once we tell Haskell that AbstractNatural can do equality
-- comparisons and how AbstractNatural is totally ordered, we
-- get other functions for free, like /= and >= and > and <
--
-- You may not need these so I've left them commented out, but
-- you should understand why they work.
--
instance Eq AbstractNatural where
  Zero == Zero = True
  Zero == S _  = False
  S _  == Zero = False
  S x  == S y  = x == y

instance Ord AbstractNatural where
  Zero <= Zero = True
  Zero <= S _  = True
  S _  <= Zero = False
  S x  <= S y  = x <= y

successorNat :: AbstractNatural -> AbstractNatural
successorNat = S

predecessorNat :: AbstractNatural -> AbstractNatural
predecessorNat Zero  = Zero
predecessorNat (S x) = x

toAbstractNat :: Integer -> AbstractNatural
toAbstractNat 0 = Zero
toAbstractNat x = S $ toAbstractNat (x - 1)

fromAbstractNat :: AbstractNatural -> Integer
fromAbstractNat Zero  = 0
fromAbstractNat (S x) = 1 + fromAbstractNat x

-- Defining Num functions on AbstractNatural, helpful for AbstractIntger. Negative values truncate at Zero.
instance Num AbstractNatural where
  x + Zero = x
  x + S y  = S (x + y)

  x * Zero = Zero
  x * S y  = x + x * y

  Zero - _    = Zero
  x    - Zero = x
  S x  - S y  = x - y

-- Figure out how you will define integers...

data AbstractInteger = P AbstractNatural | N AbstractNatural
  deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

successor :: AbstractInteger -> AbstractInteger
successor (P n    ) = P (S n)
successor (N Zero ) = P (S Zero)
successor (N (S n)) = N n

predecessor :: AbstractInteger -> AbstractInteger
predecessor (N n    ) = N (S n)
predecessor (P Zero ) = N (S Zero)
predecessor (P (S n)) = P n

-- Be sure to add type declarations to all these functions too.
negator :: AbstractInteger -> AbstractInteger
negator (P n) = N n
negator (N n) = P n

absolute :: AbstractInteger -> AbstractInteger
absolute (P n) = P n
absolute (N n) = P n

add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add (P a) (P b) = P (a + b)
add (N a) (N b) = N (a + b)
add (P a) (N b) | -- The a == b case can be handled in either.
                  a > b  = P (a - b)
                | a <= b = N (b - a)
add (N a) (P b) = add (P b) (N a)

difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference a b = add a (negator b)

multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply (P a) (P b) = P (a * b)
multiply (N a) (N b) = P (a * b)
multiply (P a) (N b) = N (a * b)
multiply (N a) (P b) = N (a * b)

-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.
instance Eq AbstractInteger where
  P a == P b = a == b
  N a == N b = a == b
  _   == _   = False

instance Ord AbstractInteger where
  N _ <= P _ = True
  P _ <= N _ = False
  N a <= N b = a <= b
  P a <= P b = a <= b

-- I feel like both of the following could be simplified a bit more.
divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide (N Zero) _ = P Zero
divide (P a) (P b) | a < b     = P Zero
                   | otherwise = add (P (S Zero)) (divide (P (a - b)) (P b))
divide (N n) (N b) = divide (P n) (P b)
divide (N a) (P b) = add (N (S Zero)) (divide (add (N a) (P b)) (P b))
divide (P a) (N b) = divide (N a) (P b)

modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo (P a) (P b) | b > a     = P a
                   | otherwise = modulo (P (a - b)) (P b)
modulo (N a) (P b) = modulo (add (N a) (P b)) (P b)
modulo (P a) (N b) = modulo (N a) (P b)
modulo (N a) (N b) = modulo (P a) (P b)

toAbstract :: Integer -> AbstractInteger
toAbstract x | x < 0     = N $ toAbstractNat (-x)
             | otherwise = P $ toAbstractNat x

fromAbstract :: AbstractInteger -> Integer
fromAbstract (P n) = fromAbstractNat n
fromAbstract (N n) = -(fromAbstractNat n)

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList = case (stack, inputList) of
  (x : _, []) -> x -- No more input, return top of stack.
  (y : x : stackRest, "+" : inputRest) ->
    evalRPNStack (add x y : stackRest) inputRest
  (y : x : stackRest, "*" : inputRest) ->
    evalRPNStack (multiply x y : stackRest) inputRest
  (y : x : stackRest, "-" : inputRest) ->
    evalRPNStack (difference x y : stackRest) inputRest
  (y : x : stackRest, "/" : inputRest) ->
    evalRPNStack (divide x y : stackRest) inputRest
  (y : x : stackRest, "%" : inputRest) ->
    evalRPNStack (modulo x y : stackRest) inputRest
  (x : stackRest, "abs" : inputRest) ->
    evalRPNStack (absolute x : stackRest) inputRest
  -- ...add more cases here...
  -- This last case handles numeric inputs, "0" "-2" "34" etc...
  (_, numStr : inputRest) ->
    evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero = P Zero

one = successor zero

two = successor one

three = successor two

four = successor three

five = successor four

six = successor five

seven = successor six

eight = successor seven

nine = successor eight

ten = successor nine

negativeOne = predecessor zero

negativeTwo = predecessor negativeOne

negativeThree = predecessor negativeTwo

negativeFour = predecessor negativeThree

negativeFive = predecessor negativeFour

negativeSix = predecessor negativeFive

negativeSeven = predecessor negativeSix

negativeEight = predecessor negativeSeven

negativeNine = predecessor negativeEight

negativeTen = predecessor negativeNine
