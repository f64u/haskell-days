{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where

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
-- instance Eq AbstractNatural where
--   Zero == Zero = True
--   Zero == S _  = False
--   S _  == Zero = False
--   S x  == S y  = x == y
--
-- instance Ord AbstractNatural where
--   Zero <= Zero = True
--   Zero <= S _  = True
--   S _  <= Zero = False
--   S x  <= S y  = x <= y
--
-- successorNat :: AbstractNatural -> AbstractNatural
-- successorNat = S
--
-- predecessorNat :: AbstractNatural -> AbstractNatural
-- predecessorNat Zero  = Zero
-- predecessorNat (S x) = x


-- Figure out how you will define integers...

data AbstractInteger = Undefined
    deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

successor :: AbstractInteger -> AbstractInteger
successor = undefined

predecessor :: AbstractInteger -> AbstractInteger
predecessor = undefined

-- Be sure to add type declarations to all these functions too.
negator    = undefined
absolute   = undefined
add        = undefined
difference = undefined
multiply   = undefined

-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.
instance Eq AbstractInteger where
    x == y = undefined
    -- add more cases here...

instance Ord AbstractInteger where
    x <= y = undefined
    -- add more cases here...

divide = undefined
modulo = undefined

toAbstract :: Integer -> AbstractInteger
toAbstract = undefined

fromAbstract :: AbstractInteger -> Integer
fromAbstract = undefined

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        -- ...add more cases here...
        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero  = undefined
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
