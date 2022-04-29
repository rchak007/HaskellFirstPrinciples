-- tests/tests.hs
module Main where
import qualified Data.Map as M
import Morse
import Test.QuickCheck

import Test.Hspec
import WordNumber
import Data.List
import Data.Char


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars
-- *Main> sample' charGen
-- "rsjhbrd2dn5"
-- *Main> charGen    ----  you cant print a Gen.. so instead need to do sample or sample'

-- <interactive>:7:1: error:
--     • No instance for (Show (Gen Char)) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it


morseGen :: Gen Morse
morseGen = elements allowedMorse



prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c)
      >>= morseToChar) == Just c)


posInt :: Int -> Int 
posInt i = if i < 0 then (i * (-1)) else i

posWordNumber :: Int -> String
posWordNumber x = wordNumber ( posInt x) 

-- 14.7 Chapter exercises
-- numbers into words 
--    see main where actual value tests are done.
-- Using QuickCheck
-- 1
half x = x / 2
halfIdentity = (*2) . half

halfProp :: Float -> Bool 
halfProp i = i == (halfIdentity i)

-- *Main> :halfProp 10
-- True
-- *Main> halfProp 102
-- True
-- *Main> halfProp 90
-- True

-- 2 
-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

iList = listOrdered [1,200,43, 50, 54000, 2]
iList2 = listOrdered [1,2,43, 500, 54000]
-- *Main> sort [1,2,43, 500, 54000]
-- [1,2,43,500,54000]

propListOrdered :: (Ord a) => [a] -> Bool
propListOrdered xs = listOrdered (sort xs)

-- 3 - 
-- we’ll test the associative and commutative properties of addition:

plusAssociative x y z =
    x + (y + z) == (x + y) + z
plusCommutative x y =
    x + y == y + x

-- Keep in mind, these properties won’t hold for types based on IEEE-754 floating point numbers, such as Float or Double.


minusAssociative x y z =
    x - (y - z) == (x - y) - z


-- 4. Now do the same for multiplication.
multAssociative x y z =
    x * (y * z) == (x * y) * z
multCommutative x y =
    x * y == y * x

-- 5 
-- We mentioned in one of the first chapters that there are some laws involving the relationships of quot to rem and
-- div to mod. Write QuickCheck tests to prove them:
-- (quot x y) * y + (rem x y) == x
-- (div x y) * y + (mod x y) == x

propQuot x y 
    | x > 0 && y > 0 = (quot x y) * y + (rem x y) == x
    | otherwise = True     -- temporarily forcing negative numbers to 0 since its intergral positive numbers
    
propDiv x y 
    |  x > 0 && y > 0 =  (div x y) * y + (mod x y) == x
    | otherwise = True     -- temporarily forcing negative numbers to 0 since its intergral positive numbers


-- 6. Is the ^ operation associative? Is it commutative? Use QuickCheck to see if the computer can contradict such an assertion.

expAssociative x y z =
    x ^ (y ^ z) == (x ^ y) ^ z
expCommutative x y =
    x ^ y == y ^ x

-- 7. Test that reversing a list twice is the same as the identity of the original list:
-- reverse . reverse == id

prop2Reverse :: Eq a => [a] -> Bool
prop2Reverse x = (reverse . reverse) x  == id x


-- 8. Write a property for the definition of $:
-- f $ a = f a
-- f . g = \x -> f (g x)



-- 9. See if these two functions are equal:
-- foldr (:) == (++)
-- foldr (++) [] == concat

--propFoldrConce :: Eq a => [a] -> [a] -> Bool 
propFoldrConce x y  = foldr (:) x y == (++) x y

-- *Main> foldr (:) [1,2,3] [4,5,6]
-- [4,5,6,1,2,3]
-- *Main> (++) [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- propFoldConcat :: Foldable t =>  t [a] -> Bool    -- reason you cant do this ?? 
propFoldConcat x = foldr (++) [] x == concat x


-- 10. Hmm. Is that so?
--f n xs = length (take n xs) == n
propLength n xs = length (take n xs) == n
-- these fail because the string can be smaller length you might pass n that is > and also n can be -ve or string can be blank.


propLength' n xs
    | n < 0 || xs == [] = True 
    | n <= (length xs) && n > 0 && xs /= [] = length (take n xs) == n     -- eliminate 
    | otherwise = True



-- 11. Finally, this is a fun one. You may remember we had you compose read and show one time to complete a “round
-- trip.” Well, now you can test that it works:
-- f x = (read (show x)) == x
propReadShow x = (read (show x)) == x

-- *Main> read (show "Rafa") :: String
-- "Rafa"
-- *Main> read (show "Rafa")      -- when you directly do w/o qualiying type it fails. 
-- *** Exception: Prelude.read: no parse
-- *Main> read "1"
-- *** Exception: Prelude.read: no parse
-- *Main> read "1" :: Int
-- 1
-- *Main> propReadShow "rafa"     -- when you do like i think it infers "rafa" is string already 
-- True
-- *Main> propReadShow 5
-- True


-- Failure
-- Find out why this property fails:
-- for a function
square x = x * x
-- Why does this property not hold?
-- Examine the type of sqrt.
squareIdentity x = (square . sqrt) x

-- *Main> x  = sqrt 10
-- *Main> x
-- 3.1622776601683795
-- *Main> x * x
-- 10.000000000000002
propSquare x = (square . sqrt) x == x
-- "propSquare x = (square . sqrt) x == x"
-- *** Failed! Falsified (after 2 tests and 2 shrinks):    
-- 0.2

-- Idempotence

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord [x] = [toUpper x]
capitalizeWord (x:xs) = toUpper x : capitalizeWord xs

twice f = f . f
fourTimes = twice . twice
propCapitalize x =
                 (capitalizeWord x
                 == twice capitalizeWord x)
                 &&
                 (capitalizeWord x
                 == fourTimes capitalizeWord x)
propSort24 x =
                 (sort x
                 == twice sort x)
                 &&
                 (sort x
                 == fourTimes sort x)
-- "propCapitalize "
-- +++ OK, passed 100 tests.
-- "propCapitalize "
-- +++ OK, passed 100 tests.
-- "propSort24 String "
-- +++ OK, passed 100 tests.
-- "propSort24 Int "
-- +++ OK, passed 100 tests.


-- Make a Gen random generator for the datatype
-- We demonstrate in this chapter how to make Gen generators for different datatypes. We are so certain you enjoyed that, we
-- are going to ask you to do it for some new datatypes:

-- 1. Equal probabilities for each:
data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = do
    oneof [return Fulse,
           return Frue]

-- *Main> sample' foolGenEqual
-- [Frue,Fulse,Fulse,Frue,Fulse,Frue,Fulse,Frue,Fulse,Fulse,Frue]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue:
data Fool' =
    Fulse'
    | Frue'
    deriving (Eq, Show)
foolGenEqual' :: Gen Fool'
foolGenEqual' = do
    frequency [ (2, return Fulse') ,
               (1, return Frue') ]
-- *Main> sample' foolGenEqual'
-- [Frue',Frue',Fulse',Fulse',Fulse',Frue',Fulse',Fulse',Fulse',Frue',Fulse']



main :: IO ()
main =  
    -- quickCheck prop_thereAndBackAgain
    do 
      print "prop_thereAndBackAgain test: "
      quickCheck prop_thereAndBackAgain
      hspec $ do
        describe "digitToWord" $ do
          it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
          it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
            -- print "???"
        describe "digits" $ do
          it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
          it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]
            -- print "???"
        describe "wordNumber" $ do
          it "one-zero-zero given 100" $ do
            wordNumber 100
               `shouldBe` "one-zero-zero"
          it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
      
      print "halfProp test: "
      quickCheck halfProp
      print "propListOrdered [Char] test: "
      quickCheck ( propListOrdered :: [Char] -> Bool)
      print "propListOrdered [Int] test: "
      quickCheck ( propListOrdered :: [Int] -> Bool)
      print "propListOrdered [Maybe Int] test: "
      quickCheck ( propListOrdered :: [Maybe Int] -> Bool)
      print "listOrdered [Int] test to FAIL: "
      quickCheck ( listOrdered :: [Int] -> Bool)
      print "Associative Plus test: "
      quickCheck ( plusAssociative :: Int -> Int -> Int -> Bool)      
      print "Commutative Plus test: "
      quickCheck ( plusCommutative :: Int -> Int -> Bool)
      print "Assoc minus test to fail: "
      quickCheck ( minusAssociative :: Int -> Int -> Int -> Bool)      
      print "Associative Multiply test: "
      quickCheck ( multAssociative :: Int -> Int -> Int -> Bool)      
      print "Commutative Multiply test: "
      quickCheck ( multCommutative :: Int -> Int -> Bool)
      print "Property Quot test: "
      quickCheck ( propQuot :: Int -> Int -> Bool) 
      print "Property Div test: "
      quickCheck ( propDiv ::  Int -> Int -> Bool)
      print "Exp Associative test to Fail: "
      quickCheck ( expAssociative :: Int -> Int -> Int -> Bool)      
      print "Exp Commutative test to Fail: "
      quickCheck ( expCommutative :: Int -> Int -> Bool)
      print "Reverse twice is id  test for Int: "
      quickCheck ( prop2Reverse ::  [Int] -> Bool)
      print "Reverse twice is id  test for Char: "
      quickCheck ( prop2Reverse ::  [Char] -> Bool)
      print "foldr (:) == (++) test for Char: Expected to FAIL "
      quickCheck ( propFoldrConce ::  [Char] -> [Char] -> Bool)
      print "foldr concat test for String: "
      quickCheck ( propFoldConcat ::  [String] -> Bool)
      print "length (take n xs) == n test: Expected to Fail "
      quickCheck ( propLength ::  Int -> String -> Bool)
      print "length (take n xs) == n test: Expected to Pass "
      quickCheck ( propLength' ::  Int -> String -> Bool)
      print "propReadShow x = (read (show x)) == x, Int"
      quickCheck ( propReadShow :: Int -> Bool)
      print "propReadShow x = (read (show x)) == x, Char"
      quickCheck ( propReadShow :: Char -> Bool)
      print "propReadShow x = (read (show x)) == x, String"
      quickCheck ( propReadShow :: String -> Bool)   
      print "propSquare x = (square . sqrt) x == x"
      quickCheck ( propSquare :: Float -> Bool) 
      print "propCapitalize "
      quickCheck ( propCapitalize :: String -> Bool) 
      print "propCapitalize "
      quickCheck ( propCapitalize :: String -> Bool) 
      print "propSort24 String "
      quickCheck ( propSort24 :: String -> Bool) 
      print "propSort24 Int "
      quickCheck ( propSort24 :: [Int] -> Bool) 

-- *Main> main
-- "prop_thereAndBackAgain test: "
-- +++ OK, passed 100 tests.

-- digitToWord
--   returns zero for 0 [✔]
--   returns one for 1 [✔]
-- digits
--   returns [1] for 1 [✔]
--   returns [1, 0, 0] for 100 [✔]
-- wordNumber
--   one-zero-zero given 100 [✔]
--   nine-zero-zero-one for 9001 [✔]

-- Finished in 0.0016 seconds
-- 6 examples, 0 failures
-- "halfProp test: "
-- +++ OK, passed 100 tests.
-- "propListOrdered [Char] test: "
-- +++ OK, passed 100 tests.
-- "propListOrdered [Int] test: "
-- +++ OK, passed 100 tests.
-- "propListOrdered [Maybe Int] test: "
-- +++ OK, passed 100 tests.
-- "listOrdered [Int] test to FAIL: "
-- *** Failed! Falsified (after 8 tests and 6 shrinks):    
-- [1,0]
-- "Associative Plus test: "
-- +++ OK, passed 100 tests.
-- "Commutative Plus test: "
-- +++ OK, passed 100 tests.
-- "Assoc minus test to fail: "
-- *** Failed! Falsified (after 2 tests and 4 shrinks):    
-- 0
-- 0
-- 1
-- "Associative Multiply test: "
-- +++ OK, passed 100 tests.
-- "Commutative Multiply test: "
-- +++ OK, passed 100 tests.
