
-- 4.1 Basic datatypes

-- numbers, characters, and lists of characters, also called strings. These are some of the standard datatypes and are built into the standard
-- library. While those are useful datatypes and cover a lot of types of values, they don’t cover every type of data. In this chapter, we will:

-- * Review types we have seen in previous chapters.
-- • Learn about datatypes, type constructors, and data constructors.
-- • Work with predefined datatypes.
-- • Learn more about type signatures and a bit about type
-- classes.

-- 4.2 What are types?

-- Expressions, when evaluated, reduce to values. Every value has a type.
-- Types are how we group a set of values together that share something in common. 
-- Sometimes that commonality is abstract; sometimes it’s a specific model of a particular concept or domain.

-- 4.3 Anatomy of a data declaration

-- ****************************************   type constructor is the name of the type and is capitalized *********************************************
-- The type constructor is the name of the type and is capitalized.

-- will start with a basic datatype to see how datatypes are structured and get acquainted with the vocabulary.


-- ****************************************   type constructor  *********************************************
-- ****************************************   data constructor  *********************************************
-- ****************************************   sum type  *********************************************

-- data Bool = False | True
-- --    [1]    [2] [3] [4]
-- 1. Type constructor for datatype Bool. This is the name of the type and shows up in type signatures.
-- 2. Data constructor for the value False.
-- 3. The pipe | indicates a sum type or logical disjunction: or. So, a Bool value is True or False.
-- 4. Data constructor for the value True.

-- This whole thing is called a data declaration.
-- ****************************************   data declaration  *********************************************
-- do not always follow precisely the same pattern—there are datatypes that use logical conjunction (and) instead of disjunction,
-- and some type constructors and data constructors may have arguments.

-- have in common is the keyword data followed by the type constructor (or the name of
-- the type that will appear in type signatures), the equals sign to denote a definition, and then data constructors (or the names of the values that inhabit term-level code).

-- Prelude> :info Bool
-- data Bool = False | True


-- ************************************ type signature makes reference to the type constructor *********************************
-- ************************************ when we use function we use the data constructors which could evaluate to another data constructor or diff one or values **************

-- If we query the type information for a function called not, we see that it takes one Bool value and returns another Bool value, 
-- so the type signature makes reference to the type constructor, or datatype name:
-- Prelude> :t not
-- not :: Bool -> Bool

-- But when we use the not function, we use the data constructors, or values:
-- Prelude> not True
-- False


-- And our expression evaluates to another data constructor, or value—in this case, the other data constructor for the same datatype.

-- Exercises: Mood swing


-- Given this datatype, answer the following questions:
-- ************************************ deriving Show ***************************************************************

-- data Mood = Blah | Woot deriving (Eq, Show)
data Mood = Blah | Woot deriving Show

-- The deriving Show part is not something we’ve explained yet. For now, all we’ll say is that when you make your own datatypes, 
-- deriving Show allows the values of that type to be printed to the screen. We’ll talk about it more when we talk about type classes in detail.

-- 1. What is the type constructor, or name of this type?                                                   -- Mood
-- 2. If the function requires a Mood value, what are the values you could possibly use?                    -- Blah / Woot
-- 3. We are trying to write a function changeMood to change Chris’s mood instantaneously. 
--    It should act like not in that, given one value, it returns the other value of the same type. 
--    So far, we’ve written a type signature 
--    changeMood :: Mood -> Woot. 
--    What’s wrong with that?                                                                               -- Woot is not type constructor , its a data constructor.
-- 4. Now we want to write the function that changes his mood. Given an input mood, 
-- it gives us the other one. Fix any mistakes and complete the function:
-- changeMood Mood = Woot
-- changeMood _ = Blah
x1 = Blah


-- ************************************ deriving Eq ***************************************************************
-- changeMood :: Mood -> Mood
-- changeMood x = if x == Blah               -- Needs the Eq in deriving
--                then Woot 
--                else Blah

changeMood1 :: Mood -> Mood               -- Dont need Eq.
changeMood1 Blah = Woot
changeMood1 Woot = Blah

changeMood2 :: Mood -> Mood
changeMood2 Blah = Woot
changeMood2 _ = Blah


-- 4.4 Numeric types

-- important to understand that Haskell does not use only one type of number

-- ************************************ Integral numbers - Int / Integer and Word ***************************************************************
-- Integral numbers These are whole numbers, positive and negative.

-- ************************************ fixed-precision integer ***************************************************************

-- 1. Int: This type is a fixed-precision integer. By “fixed precision,” we mean it has a range, with a maximum and a
--    minimum, and so it cannot be arbitrarily large or small more about this in a moment.
-- 2. Integer: This type is also for integers, but this one supports arbitrarily large (or small) numbers.
-- 3. Word: This is also a fixed-precision integer. Unlike Int, the smallest or lowest possible value expressible with a Word
--    is the number zero. Word is suitable when you want to express whole digits that don’t include negative numbers.

int1 :: Int        -- can be negative
int1 = -5

-- word1 :: Word
-- word1 = -2     -- error - Literal -2 is out of the Word range 0..18446744073709551615



-- ************************************ Fractional: Float, Double, Rational, Fixed, & Scientific  ***************************************************************
-- Fractional

-- These are not integers. Fractional values include the following five types:


-- 1. Float: This is the type used for single-precision floating point numbers. Fixed-point number representations have immutable numbers of digits assigned for the parts
-- of the number before and after the decimal point. Sometimes fixed-point is called fixed-precision. In contrast, floating point can shift how many bits it uses to represent
-- numbers before or after the decimal point. This flexibility does, however, mean that floating point arithmetic violates some common assumptions and should only be
-- used with great care. Generally, floating point numbers should not be used at all in business applications.

-- 2. Double: This is a double-precision floating point number type. It has twice as many bits with which to describe numbers as the Float type.

-- 3. Rational: This is a fractional number that represents a ratio of two integers. 
-- The value 1 / 2 :: Rational itself carries two Integer values, the numerator 1 and the denominator 2, and represents a ratio of 1 to 2. Rational is
-- arbitrarily precise but not as efficient as Scientific.

-- 4. Fixed: This is a fixed-point (or fixed-precision) type that can represent varying numbers of decimal points, depending on which type you choose. 
-- If your type is Fixed E2, then your values can track up to two digits after the decimal point. If the type is Fixed E9, then it could be up to nine
-- digits after the decimal point. The base library provides E0, E1, E2, E3, E6, E9, and E12. It isn’t too much trouble to
-- add your own resolution if you require a different amount of precision.

-- 5. Scientific: This is a space efficient and almost arbitrary precision scientific number type. Scientific numbers are represented using scientific notation. 
-- It stores the coefficient as an Integer and the exponent as an Int. Since Int isn’t arbitrarily large, 
-- there is technically a limit to the size of number you can express with Scientific, but hitting that limit is unlikely. Scientific is available in a
-- library2 and can be installed using cabal install or stack install.

-- ************************************ type class Num  ***************************************************************
-- These numeric datatypes all have instances of a type class called Num.

-- The Num type class is what provides your standard +, -, and * operators, along with a few others.
-- Any type that has an instance of Num can be used with those functions. 
-- An instance defines how the functions work for that specific type. We will talk about type classes in much more detail soon.


-- Integral numbers


-- following are integral numbers:
-- 1 2 199 32442353464675685678

-- following are not integral numbers:
-- 1.3 1/2


-- Integer
-- The numbers of type Integer are the sorts of numbers we’re used to working with in arithmetic equations that involve whole numbers. 
-- They can be positive or negative, and Integer extends as far in either direction as one needs them to go.

-- ************************************ Bool we can write explicilty data constructors out but Integer we cannot   ***************************************************************
-- The Bool datatype only has two possible values, so we can list them explicitly as data constructors. In the case of Integer, and
-- most numeric datatypes, these data constructors are not writtenout, because they include an infinite series of whole numbers.
-- We’d need infinite data constructors stretching up and down from zero. Hypothetically, we could represent Integer as a sum of three cases: 
-- recursive constructors headed toward negative infinity, zero, and recursive constructors headed toward positive infinity. 
-- This representation would be terribly inefficient, so there’s some GHC magic sprinkled on numbers, instead.


-- ************************************ Danger of using Int   ***************************************************************
-- Why do we have Int?
-- The Int numeric types are artifacts of what computer hardware has supported natively over the years. 
-- Most programs should use Integer, not Int, unless the limitations of the type are understood and the additional performance makes a difference.
-- The danger of Int and the related types Int8, Int16, et al. is that they cannot express arbitrarily large quantities of information.


-- Prelude> 128 :: Int8
-- Literal 128 is out of the
-- Int8 range -128..127
-- If you are trying to write a large negative
-- literal, use NegativeLiterals
-- -128
-- Prelude> (127 + 1) :: Int8
-- -128

-- using maxBound and minBound from the Bounded type class. 


-- Prelude> import GHC.Int
-- Prelude> :t minBound
-- minBound :: Bounded a => a
-- Prelude> minBound :: Int8
-- -128
-- Prelude> minBound :: Int16
-- -32768

-- range of values we can represent with an Int8 is -128 to 127.


-- Word

-- pg 150 
-- Now use your REPL to determine whether 'a' or 'A' is greater.
-- *Main GHC.Int> 'a' < 'A'
-- False


-- 4.9 Chapter exercises

-- Ex 8, pg 169
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == rx 
                 where rx = reverse x

-- Ex 9, pg 170
myAbs :: Integer -> Integer
myAbs x = if x > 0
          then x
          else x*(-1)
-- *Main> myAbs 5
-- 5
-- *Main> myAbs (-5)
-- 5

-- ***** existing abs but above we implemented on our own
-- Prelude> abs 10
-- 10
-- Prelude> abs (-10)
-- 10
-- Prelude> abs 10.01
-- 10.01
-- Prelude> abs (-10.01)
-- 10.01

-- Ex 10, pg 170

-- 10. Fill in the definition of the following function, using fst
-- and snd:

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y) , (fst x, fst y))

-- *Main> f (1,2) (3,4)
-- ((2,4),(1,3))

-- Correcting syntax


x = (+)

stringF1 xs = w `x` 1
       where w = length xs

-- *Main> stringF1 "Chuck"
-- 6

-- (\x -> x) :: a -> a
f1 :: Integer -> Integer
f1 = \x -> x
-- *Main> f1 4
-- 4

-- Ex 3 - pg 171
f3 (a, b) = a
-- *Main> f3 (1,2)
-- 1

funcDiv :: Float -> Float -> Float -> Float
funcDiv a b c = a / b / c 
-- *Main> funcDiv 1000 100 10
-- 1.0

-- for Emurgo student issue.
caseExp :: String -> String
caseExp x = case x of 
                 [] -> "the string is" ++ "empty" 
                 [x] -> "the string is" ++ "charred x"
                 (x:_) -> "the string is" ++ "looong"


caseExp' :: String -> String
caseExp' x = "the string is" ++ case x of 
                                     [] -> "the string is" ++ "empty" 
                                     [x] -> "the string is" ++ "charred x"
                                     (x:_) -> "the string is" ++ "looong"




mixedFunc a b c d = ((a + b) * c ) - d
-- *Main> mixedFunc 10 20 30 40
-- 860

-- (((a + b) * c ) - d)) 10 20 30 40
-- [ a := 10 ]
-- (((10 + b)  * c ) - d ) 20 30 40   
-- [ b := 20 ]  
-- (((10 + 20) * c )- d)) 30 40         
-- (30 * c ) - d)  30 40 
-- [c := 30]
-- ((30 * 30) - d) 40
-- (900 - d) 40
-- [ d:= 40]
-- 900 -  40  
-- 860


--  Exercises: Type arguments - pg 204 Chapter  4

--  1
-- *Main> u = undefined
-- *Main> f :: a -> a -> a -> a; f = u
-- *Main> x :: Char; x = u
-- *Main> :t f x
-- f x :: Char -> Char -> Char

-- 2
-- *Main> g :: a -> b -> c -> b; g = u
-- *Main> :t g 0 'c' "woot"
-- g 0 'c' "woot" :: Char


--3
-- *Main> h :: (Num a, Num b) => a -> b -> b ; h = u
-- *Main> :t h 1.0 2
-- h 1.0 2 :: Num b => b

-- 4
-- *Main> :t h 1 (5.5 :: Double)
-- h 1 (5.5 :: Double) :: Double

-- 5
-- *Main> jackal :: (Ord a, Eq b) => a -> b -> a ; jackal = u
-- *Main> :t jackal "keyboard" "has the word jackal in it"
-- jackal "keyboard" "has the word jackal in it" :: [Char]

-- 6
-- *Main> :t jackal "keyboard"
-- jackal "keyboard" :: Eq b => b -> [Char]


-- 7 
-- Prelude> kessel :: (Ord a, Num b) => a -> b -> a ; kessel = u
-- Prelude> :t kessel 1 2
-- kessel 1 2 :: (Ord a, Num a) => a
--- i think Ord is much wider class than Num but why is a Num constrained?
-- Ans -- Because we already called as Num in the application.!! because the 1st argument is instanciating the ord into Num class.

-- 8 
-- Prelude> :t kessel 1 (2 :: Integer)
-- kessel 1 (2 :: Integer) :: (Ord a, Num a) => a

-- 9 
-- Prelude> :t kessel (1 :: Integer) 
-- kessel (1 :: Integer) 2 :: Integer

-- Exercises: Parametricity 
-- pg 213

-- 1
-- funcIdType :: (Num a ) => a -> a     -- You have to supply Type Contraint
-- funcIdType :: a -> a    -- cant do with this
-- funcIdType x = x + 1


-- 2
funcPar2 :: a -> a -> a
funcPar2 x y = x y 

funcPar3 :: (a->a) -> a
funcpar3 f = f 10


funcPar2' :: (Num => a) a -> a -> a
funcPar2' x y = x + y 

funcPar3' :: (a->a) -> a
funcpar3' f = f 













