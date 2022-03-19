

-- Exercises: Grab bag

-- Ex 1
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

mTh''' = \x -> \y -> \z -> x * y * z

-- *Main> mTh 3 8 9 
-- 216
-- *Main> mTh' 3 8 9
-- 216
-- *Main> mTh'' 3 8 9
-- 216
-- *Main> mTh''' 3 8 9
-- 216

-- Ex 3
--3a
-- we’ll practice writing anonymous lambda syntax

-- Rewrite the f function in the where clause:
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1
-- *Main> addOneIfOdd 2
-- 2
-- *Main> addOneIfOdd 3
-- 4

addOneIfOdd' n = case odd n of
    True -> (\n -> n + 1) n
    False -> n
-- *Main> addOneIfOdd' 2
-- 2
-- *Main> addOneIfOdd' 3
-- 4


-- Ex 3b
-- Rewrite the following to use anonymous lambda syntax:
addFive x y = (if x > y then y else x) + 5
-- *Main> addFive 23 21
-- 26
-- *Main> addFive 18 30
-- 23

addFive' = \x -> \y -> (if x > y then y else x) + 5
-- *Main> addFive' 23 21
-- 26
-- *Main> addFive' 18 30
-- 23

-- Ex 3c
-- Rewrite the following so that it doesn’t use anonymous lambda syntax:
mflip f = \x -> \y -> f y x
mflip' f x y = f y x


-- Wrong order
isItTwo :: Integer -> Bool
isItTwo _ = False
isItTwo 2 = True

-- bottom - incompete pattern match 
isItTwo' :: Integer -> Bool
isItTwo' 2 = True
-- *Main> isItTwo' 3
-- *** Exception: purple-7-FuncPatterns.hs:73:1-17: Non-exhaustive patterns in function isItTwo'


-- Prelude> :set -Wall
-- Prelude> :{
-- Prelude| let isItTwo :: Integer -> Bool
-- Prelude|     isItTwo 2 = True
-- Prelude| :}

-- <interactive>:339:5: warning: [-Wname-shadowing]
--     This binding for ‘isItTwo’ shadows the existing binding
--       defined at <interactive>:333:5

-- <interactive>:339:5: warning: [-Wincomplete-patterns]
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘isItTwo’:
--         Patterns not matched: p where p is not one of {2}



-- -- registeredUser2.hs

-- matchingTuples1.hs



-- Exercises: Variety pack

f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))

f (a,b,c) (d,e,f) = ((a,d), (c,f))



-- greetIfCool3.hs

-- Exercises: Case practice
-- Ex 1
functionC x y = if (x > y) then x else y

functionCase x y = case (x > y) of
	               True -> x
	               False -> y
-- *Main> functionC 5 2
-- 5
-- *Main> functionC 5 14
-- 14
-- *Main> functionCase 5 14
-- 14
-- *Main> functionCase 5 1
-- 5

-- Ex 2
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2case n = case even n of
                   True -> (n+2) 
                   False -> n
-- *Main> ifEvenAdd2 6
-- 8
-- *Main> ifEvenAdd2 7
-- 7
-- *Main> ifEvenAdd2case  6
-- 8
-- *Main> ifEvenAdd2case  7
-- 7


-- Ex 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- *Main> nums 10
-- 1
-- *Main> nums (-10)
-- -1
-- *Main> nums 0
-- *** Exception: purple-7-FuncPatterns.hs:(147,3)-(149,11): Non-exhaustive patterns in case     --- before adding EQ 

-- *Main> :r
-- [1 of 1] Compiling Main             ( purple-7-FuncPatterns.hs, interpreted )
-- Ok, one module loaded.
-- *Main> nums 0
-- 0


-- Exercises: Artful dodgy
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- *Main> dodgy 1 0
-- 1
-- *Main> dodgy 1 1
-- 11
-- *Main> dodgy 2 2
-- 22
-- *Main> dodgy 1 2
-- 21
-- *Main> dodgy 2 1
-- 12
-- *Main> oneIs
-- oneIsOne  oneIsTwo
-- *Main> oneIsOne 1
-- 11
-- *Main> oneIsOne 2
-- 21
-- *Main> oneIsTwo 1
-- 21
-- *Main> oneIsTwo 2
-- 22
-- *Main> oneIsOne 3
-- 31
-- *Main> oneIsTwo 3
-- 23




-- Exercises: Guard duty

-- Ex 1
avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
    -- | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

-- Ex 3
pal xs
    | xs == reverse xs = True
    | otherwise = False

-- Ex 6
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

-- *Main> :t numbers
-- numbers :: (Ord a, Num a, Num p) => a -> p


-- arith2.hs



-- 7.11 Chapter exercises

-- Multiple choice

f1 :: Ord a => a -> a -> Bool
f1 a b = a > b
-- *Main> f1 3 4
-- False
-- *Main> f2 = f1 3
-- *Main> :t f2
-- f2 :: (Ord a, Num a) => a -> Bool

ft :: a -> a
ft x = x


-- Let’s write code

-- Ex 1 

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDivMod :: Integral a => a -> a
tensDivMod x = d
    where (x1,_) = x `divMod` 10
          (_,d) = x1 `divMod` 10


tensDivMod100 :: Integral a => a -> a
tensDivMod100 x = d
    where (x1,_) = x `divMod` 100
          (_,d) = x1 `divMod` 10


-- Ex 2 - 

foldBool :: (Ord a, Eq a) => a -> a -> Bool -> a
foldBool a b c = case ( a > b && c ) of 
                 True -> a 
                 False -> b
-- did not write all conditions actually here..but below guard i wrote all


foldBoolG :: (Ord a, Eq a) => a -> a -> Bool -> a
foldBoolG a b c  
	               | a > b && c = a 
	               | b > a && c = b 
                   | a > b = b
                   | b > a = a 
                   | otherwise = a

-- *Main> foldBoolG 7 31 False
-- 7
-- *Main> foldBoolG 7 31 True
-- 31
-- *Main> foldBoolG 47 31 False
-- 31
-- *Main> foldBoolG 47 31 True
-- 47


foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y


-- Ex 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) =  (f(a),c)

-- Ex 4 - 
-- arith4.hs

-- Definitions

data SumOfThree a b c =
    FirstPossible a
    | SecondPossible b
    | ThirdPossible c
    deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2

sumToInt' :: SumOfThree a b c -> Integer
sumToInt' (FirstPossible _) = 0
sumToInt' _ = 1







