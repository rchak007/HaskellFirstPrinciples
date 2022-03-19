
import qualified Data.List as L
import Data.Char
--import qualified Data.Map as Map

eftBool :: Bool -> Bool -> [Bool]

eftBool False True = [False, True]
eftBool False False = [False]
eftBool True False = error "not allowed"
eftBool True True = [True]
-- eftBool = undefined
-- *Main> eftBool False True
-- [False,True]
-- *Main> eftBool False False
-- [False]
-- *Main> eftBool True False
-- *** Exception: not allowed
-- CallStack (from HasCallStack):
--   error, called at purple-9-Lists.hs:7:22 in main:Main
-- *Main> eftBool True True
-- [True]


eftOrd :: Ordering
    -> Ordering
    -> [Ordering]
eftOrd x y 
    | x  == y = [x]
    | x > y = error " range from lower to higher"
    | otherwise = x : eftOrd (succ x) y
-- *Main> eftOrd LT GT
-- [LT,EQ,GT]
-- *Main> eftOrd LT EQ
-- [LT,EQ]
-- *Main> eftOrd EQ GT
-- [EQ,GT]
-- *Main> eftOrd EQ LT
-- *** Exception:  range from lower to higher
-- CallStack (from HasCallStack):
--   error, called at purple-9-Lists.hs:27:15 in main:Main

eftInt :: Int -> Int -> [Int]
eftInt x y 
    | x  == y = [x]
    | x > y = error " range from lower to higher"
    | otherwise = x : eftInt (succ x) y
-- eftInt = undefined
-- *Main> eftInt 3 10
-- [3,4,5,6,7,8,9,10]
-- *Main> eftInt 10 3
-- *** Exception:  range from lower to higher
-- CallStack (from HasCallStack):
--   error, called at purple-9-Lists.hs:20:15 in main:Main
-- *Main> eftInt 10 10
-- [10]


eftChar :: Char -> Char -> [Char]
eftChar x y 
    | x  == y = [x]
    | x > y = error " range from lower to higher"
    | otherwise = x : eftChar (succ x) y
-- *Main> eftChar 'a' 'g'
-- "abcdefg"
-- *Main> eftChar 'a' 'z'
-- "abcdefghijklmnopqrstuvwxyz"
-- *Main> eftChar 'a' 'a'
-- "a"
-- *Main> eftChar 'z' 'a'
-- "*** Exception:  range from lower to higher
-- CallStack (from HasCallStack):
--   error, called at purple-9-Lists.hs:59:15 in main:Main



eftGen :: (Enum a, Ord a) => a -> a -> [a]     -- Dont need Eq a cause Ord a covers it , Need Enum cause i used succ? Need Ord cause of > and == 
eftGen x y 
    | x  == y = [x]
    | x > y = error " range from lower to higher"
    | otherwise = x : eftGen (succ x) y
-- *Main> eftGen 'a' 'z'
-- "abcdefghijklmnopqrstuvwxyz"
-- *Main> eftGen LT GT
-- [LT,EQ,GT]
-- *Main> eftGen 1 10
-- [1,2,3,4,5,6,7,8,9,10]





-- Exercises: Thy fearful symmetry

--Ex 1 
-- Using takeWhile and dropWhile, write a function that takes a string and returns a list of strings, using spaces to separate
-- the elements of the string into words, as in the following sample:

-- myWords "sheryl wants fun"
-- ["sheryl", "wants", "fun"]

myWords :: String -> [String]
myWords [] = []
myWords a = one : myWords two  where
    one = takeWhile (> ' ') a 
    two = drop 1 (dropWhile (> ' ') a)
-- *Main> myWords "sheryl wants fun"
-- ["sheryl","wants","fun"]

-- Ex 2

-- write a function that takes a string and returns a list of strings, using newline separators to break up the string
-- as in the following (your job is to fill in the undefined function):
-- module PoemLines where
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
   \ symmetry?"
sentences = firstSen ++ secondSen
        ++ thirdSen ++ fourthSen
-- This is the result that putStrLn sentences should print:
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?


-- *Main> sentences 
-- "Tyger Tyger, burning bright\nIn the forests of the night\nWhat immortal hand or eye\nCould frame thy fearful symmetry?"
-- *Main> putStrLn sentences 
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this:
-- myLines :: String -> [String]
-- myLines = undefined


-- We want myLines sentences to equal:
-- shouldEqual =
-- [ "Tyger Tyger, burning bright"
-- , "In the forests of the night"
-- , "What immortal hand or eye"
-- , "Could frame thy fearful symmetry?"
-- ]

-- The main function here is a small test to ensure you’ve
-- written your function correctly:
-- main :: IO ()
-- main =
-- print $
-- "Are they equal? "
-- ++ show (myLines sentences
--     == shouldEqual)

myLines :: String -> [String]
myLines [] = []
myLines x = one : myLines two where
    one = takeWhile (/= '\n') x                 -- split the first part till back slash
    two = drop 1 (dropWhile (/= '\n') x)
   

-- *Main> myLines sentences 
-- ["Tyger Tyger, burning bright","In the forests of the night","What immortal hand or eye","Could frame thy fearful symmetry?"]

-- Ex 3 - 
-- Now, let’s look at what those two functions have in common. Try writing a new function that parameterizes the
-- character you’re breaking the string argument on and rewrite myWords and myLines using that parameter.

myParameterized :: (Char -> Bool) -> String -> [String]
myParameterized condFunc [] = []
myParameterized condFunc x = one : myParameterized condFunc two where
    one = takeWhile condFunc x                 -- split the first part till back slash
    two = drop 1 (dropWhile condFunc x)

-- *Main> myParameterized (/= '\n') sentences
-- ["Tyger Tyger, burning bright","In the forests of the night","What immortal hand or eye","Could frame thy fearful symmetry?"]
-- *Main> myParameterized (> ' ') "sheryl wants fun"
-- ["sheryl","wants","fun"]


-- Exercises: Comprehend thy lists
mySqr' = [x^2 | x <- [1..10]]
-- *Main> mySqr
-- [1,4,9,16,25,36,49,64,81,100]

-- *Main> [x | x <- mySqr, rem x 2 == 0]
-- [4,16,36,64,100]

mySqrTup = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- *Main> mySqrTup 
-- [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

-- *Main> take 5 mySqrTup 
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]


-- Exercises: Square cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- Ex1 - 
-- First write an expression that will make tuples of the outputs of mySqr and myCube.
myTup1 = [(x,y) | x <- mySqr, y <- myCube]

-- *Main> myTup1
-- [(1,1),(1,8),(1,27),(1,64),(1,125),(4,1),(4,8),(4,27),(4,64),(4,125),(9,1),(9,8),(9,27),(9,64),(9,125),(16,1),(16,8),(16,27),(16,64),(16,125),(25,1),(25,8),(25,27),(25,64),(25,125)]

-- Ex 2. Now, alter that expression so that it only uses the x and y values that are less than 50.
myTup2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
-- *Main> myTup2
-- [(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]

-- Ex 3 - Apply another function to that list comprehension to determine how many tuples inhabit your output list.
myTup3 x = take x myTup2         -- skipping type declaration as we dont need to for simple stuff

-- *Main> myTup2
-- [(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]
-- *Main> myTup3 5
-- [(1,1),(1,8),(1,27),(4,1),(4,8)]



-- Prelude> blah = enumFromTo 'a' 'z'
-- Prelude> :sprint blah
-- blah = _

-- Exercises: Bottom madness
-- Will it blow up?
-- Will the following expressions return a value or be ⊥?
-- Ex 1. 
-- [x^y | x <- [1..5], y <- [2, undefined]]
-- Will Blow up since x^y, forced y to be evaluated and it cannot evaluate undefied here
-- Prelude> [x^y | x <- [1..5], y <- [2, undefined]]
-- [1,*** Exception: Prelude.undefined


-- Ex 2. 
-- take 1 $
--     [x^y | x <- [1..5], y <- [2, undefined]]
-- This can take 1 since the very first one 1^2 can be evaluated
-- Prelude> take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- [1]

-- Ex 3
-- sum [1, undefined, 3]
-- this will blow up since Sum needs to evaluate the values too 
-- Prelude> sum [1, undefined, 3]
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):

-- Ex4 - 
--  length [1, 2, undefined]
-- this would be ok since length does not evaluate value.
-- Prelude> length [1, 2, undefined]
-- 3
-- Prelude> length [1, 2, undefined, 3]
-- 4

-- Ex 5
-- length $ [1, 2, 3] ++ undefined
-- Thhs will bomb since here undefined is not giving the spine of a list yet.
-- Prelude> length $ [1, 2, 3] ++ undefined
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):

-- Ex 6 - 
-- take 1 $ filter even [1, 2, 3, undefined]
-- this will be ok since its take 1 and there is an even before we hit undefined.
-- Prelude> take 1 $ filter even [1, 2, 3, undefined]
-- [2]

-- Ex 7 - 
-- take 1 $ filter even [1, 3, undefined]
-- this will bomb - it cant get even and will try evaluate undefined for value. 
-- Prelude> take 1 $ filter even [1, 3, undefined]
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):

-- Ex 8 - 
-- take 1 $ filter odd [1, 3, undefined]
-- this will be fine
-- Prelude> take 1 $ filter odd [1, 3, undefined]
-- [1]

-- Ex 9 
-- take 2 $ filter odd [1, 3, undefined]
-- will be ok since it gets 2 odd before undefined
-- Prelude> take 2 $ filter odd [1, 3, undefined]
-- [1,3]

-- Ex 10
-- take 3 $ filter odd [1, 3, undefined]
-- Will bomb 
-- Prelude> take 3 $ filter odd [1, 3, undefined]
-- [1,3*** Exception: Prelude.undefined
-- CallStack (from HasCallStack):


-- Intermission: Is it in normal form?
-- For each expression below, determine whether it’s in:
-- 1. Normal form, which implies weak head normal form.
-- 2. Weak head normal form only.
-- 3. Neither.
-- Remember that an expression cannot be in normal form or weak head normal form if the outermost part of the expression
-- isn’t a data constructor. It can’t be in normal form if any part of the expression is unevaluated:

-- 1. [1, 2, 3, 4, 5]     -- is in Normal form and WHNF too 
-- 2. 1 : 2 : 3 : 4 : _   -- is in WHNF



-- Exercises: More bottoms pg 504
-- As always, we encourage you to try figuring out the answers before you enter them into your REPL:

-- 1. Will the following expression return a value or be ⊥?
-- take 1 $ map (+1) [undefined, 2, 3]     -- will error since 1st item is undefined
-- Prelude> take 1 $ map (+1) [undefined, 2, 3]
-- [*** Exception: Prelude.undefined
-- CallStack (from HasCallStack):


-- 2. Will the following expression return a value?
tx1 = take 1 $ map (+1) [1, undefined, 3]     -- will be fine since take 1 evaluates up to only 1st element,
-- Prelude> take 1 $ map (+1) [1, undefined, 3]
-- [2]

-- 3. Will the following expression return a value?
tx2 = take 2 $ map (+1) [1, undefined, 3]    -- will error since take 2 needs 2 items evaluated
-- Prelude> take 2 $ map (+1) [1, undefined, 3]
-- [2,*** Exception: Prelude.undefined
-- CallStack (from HasCallStack):


-- 4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard
-- English and then test it out in the REPL to make sure you are correct:
itIsMystery xs = map (\x -> elem x "aeiou") xs
-- elem will return True/False if the element is in "aeiou" 
-- map will check each element in xs to see which ones are vowels.
-- Also the type will resolve to string since the "aeiou" is a string. 
-- elem is more polymorphic but checking against string "aeiou" will make it take a char to Bool and look in string which is foldable
-- Prelude> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- So finally itIsMystery will take a String return True for every vowel and False for not. Also since its a list, it will return a list of Bool.

-- 5. What will be the result of the following functions:
-- 5a) 
x1 = map (^2) [1..10]      -- 2, 4, 8, 16 ....
-- Prelude> map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

-- 5b) 
x2 = map minimum [[1..10], [10..20], [20..30]]
-- Prelude> map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]
-- n.b. minimum is not the same function as the min function that we used before

-- 5c)
x3 = map sum [[1..5], [1..5], [1..5]]
-- Prelude> map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]

-- 6. Back in Chapter 7, you wrote a function called foldBool. That function exists in a module known as Data.Bool and
-- is called bool. Write a function that does the same (or similar, if you wish) as the map if-then-else function you
-- saw above but uses bool instead of the if-then-else syntax.
-- Your first step should be bringing the bool function into scope by typing import Data.Bool at your REPL prompt

-- Prelude> map (\x -> if x == 3 then (-x) else (x)) [1..10]
-- [1,2,-3,4,5,6,7,8,9,10]


foldBoolG :: (Ord a, Num a) => Bool -> a -> a -> a         -- I had to change the order from the original Bool 
foldBoolG  cBool bcons a
               | a /= bcons && cBool = a 
               | a == bcons = (-a)
               | otherwise = a

myMapList = map (foldBoolG True 3) [1..10]

-- *Main> myMapList 
-- [1,2,-3,4,5,6,7,8,9,10]


-- Exercises: Filtering

-- 1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1–30?
filter1 f = [x | x <- [1..30], (rem x f) == 0]
-- *Main> filter1 3
-- [3,6,9,12,15,18,21,24,27,30]

-- 2. Recalling what we learned about function composition, how could we compose the above function with the length
-- function to tell us how many multiples of 3 there are between 1 and 30?
-- length :: [Int] -> Int  (in our case)
-- filter1 [Int] -> Int
-- In Haskell the precedence of an ordinary function call (white space, usually) is of 10. 
-- *Main> :i (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
-- infixr 9 .

lenFilt = length.filter1 $ 3
-- *Main> lenFilt 
-- 10

-- 3. Next, we’re going to work on removing all articles (“the,” “a,” and “an”) from sentences. You want to get to something
-- that works like this:
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]
-- You may recall that earlier in this chapter, we asked you to write a function that separates a string into a list of
-- strings by separating them at spaces. That is a standard library function called words. You may consider starting this
-- exercise by using words (or your own version, of course).

-- str = "the brown dog was a goof"
myFilter :: String -> [String]
myFilter str = [x | x <- words str , x /= "the", x /= "a", x /= "an"]


-- Zipping exercises

-- 1. Write your own version of zip, and ensure it behaves the same as the original:
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []             
myZip _ [] = []
-- myZip [a] (b:_) = [(a,b)]     -- Ends when a is last element -- dont need this since a:as already handles it.
-- myZip (a:_) [b] = [(a,b)]     -- Ends when b is last element
myZip (a:as) (b:bs) = (a,b) : myZip as bs

-- *Main> myZip [10, 5, 34, 9] [6, 8, 12, 7]
-- [(10,6),(5,8),(34,12),(9,7)]
-- *Main> myZip [10, 5, 34, 9] [6, 8, 12]
-- [(10,6),(5,8),(34,12)]
-- *Main> myZip [10, 5, 34] [6, 8, 12, 7]
-- [(10,6),(5,8),(34,12)]
-- *Main> myZip [10] [6, 8, 12, 7]
-- [(10,6)]
-- *Main> myZip [] [6, 8, 12, 7]
-- []

myTest1 :: Num b => [a] -> [(a,b)]
myTest1 [] = []
myTest1 (a:as) = (a,1) : myTest1 as
-- *Main> myTest1 [1,2]
-- [(1,1),(2,1)]
-- *Main> myTest1 [1]
-- [(1,1)]



-- 2. Do what you did for zip but now for zipWith:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith func1 [] _ = []             
myZipWith func1 _ [] = []
myZipWith func1 (a:as) (b:bs) = func1 a b : myZipWith func1 as bs

-- *Main> myZipWith (+) [10, 5, 34, 9] [6, 8, 12, 7]
-- [16,13,46,16]
-- *Main> myZipWith (+) [10, 5, 34, 9] [6, 8]
-- [16,13]
-- *Main> myZipWith (+) [10, 5] [6, 8, 12, 7]
-- [16,13]


func2Tup :: a -> b -> (a,b)
func2Tup a b = (a,b)

-- *Main> myZipWith func2Tup [10, 5] [6, 8, 12, 7]
-- [(10,6),(5,8)]
-- *Main> myZipWith func2Tup [10, 5, 34, 9, 22] [6, 8, 12, 7]
-- [(10,6),(5,8),(34,12),(9,7)]



-- 9.12 Chapter exercises

-- *Main> import Data.Char
-- *Main Data.Char> :t isUpper
-- isUpper :: Char -> Bool
-- *Main Data.Char> isUpper 'a'
-- False
-- *Main Data.Char> isUpper 'A'
-- True


-- *Main Data.Char> :t toUpper
-- toUpper :: Char -> Char
-- *Main Data.Char> toUpper 'x'
-- 'X'
-- *Main Data.Char> toUpper 'Z'
-- 'Z'



-- 2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of
-- a String? Write that function such that, given the input "HbEfLrLxO", your function will return "HELLO".
myToUpper s = [ x | x <- s , isUpper x == True ]
-- *Main Data.Char> myToUpper "HbEfLrLxO"
-- "HELLO"

-- 3. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given
-- the argument "julie", it will return "Julie".

myFirstCap [] = []
myFirstCap (x:xs) = toUpper x : xs
-- *Main Data.Char> myFirstCap "julie"
-- "Julie"

-- 4. Now make a new version of that function that is recursive, such that if you give it the input "woot", it will holler back
-- at you "WOOT". The type signature won’t change, but you will want to add a base case.
myCap [] = []
myCap (x:xs) = toUpper x : myCap xs
-- *Main Data.Char> myCap "woot"
-- "WOOT"

-- 5. To do the final exercise in this section, we’ll need another standard function for lists called head. Query the type
-- of head, and experiment with it to see what it does. Now write a function that will capitalize the first letter of a
-- String and return only that letter as the result.

myCapHead :: [Char] -> Char
myCapHead [] = error " cant be null"
-- myCapHead "" = error " cant be null"
myCapHead x = toUpper (head x )
-- *Main Data.Char> myCapHead "rafa"
-- 'R'

-- 6. Cool. Good work. Now rewrite it as a composed function.
-- Then, for fun, rewrite it point-free.
myCapHeadC :: [Char] -> Char
myCapHeadC [] = error " cant be null"
-- myCapHeadC "" = error " cant be null"
myCapHeadC x = toUpper.head $ x 
-- *Main Data.Char> myCapHeadC "rafa"
-- 'R'

-- Point-free
myCapHeadPF = toUpper.head
-- *Main Data.Char> myCapHeadPF "rafa"
-- 'R'




-- Ciphers

-- *Main Data.Char> ord 'a'
-- 97
-- *Main Data.Char> ord 'z'
-- 122
-- *Main Data.Char> chr 97
-- 'a'
-- *Main Data.Char> chr 122
-- 'z'
myCipher :: [Char] -> Int -> [Char]
myCipher [] i = []
myCipher [x] i = [chr ((ord x) + i)]
myCipher (x:xs) i = chr ((ord x) + i) : myCipher xs i



-- Page - 516 - need to do from here next exercises 




