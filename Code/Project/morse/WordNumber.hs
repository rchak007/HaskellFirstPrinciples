module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
    | n==1 = "one"
    | n==2 = "two"
    | n==3 = "three"
    | n==4 = "four"
    | n==5 = "five"
    | n==6 = "six"
    | n==7 = "seven"
    | n==8 = "eight"
    | n==9 = "nine"
    | n==0 = "zero"

div10 :: Int -> Int 
div10 n = div n 10

mod10 :: Int -> Int 
mod10 n = mod n 10

digits :: Int -> [Int]
digits n = if (div10 n > 0) then ( digits (div10 n) ++  [mod10 n])   else [mod10 n]
-- *WordNumber> digits 42021
-- [4,2,0,2,1]

wordNumber :: Int -> String
-- wordNumber n = undefined
wordNumber n = concat ( intersperse "-" (map digitToWord (digits n)) )

-- *WordNumber> wordNumber 12324546
-- "one-two-three-two-four-five-four-six"
-- *WordNumber> wordNumber 2021
-- "two-zero-two-one"

-- *WordNumber> map digitToWord (digits 42021)
-- ["four","two","zero","two","one"]
-- *WordNumber> intersperse "-" (map digitToWord (digits 42021))
-- ["four","-","two","-","zero","-","two","-","one"]
-- *WordNumber> concat ( intersperse "-" (map digitToWord (digits 42021)) )
-- "four-two-zero-two-one"
ex1 = concat ( intersperse "-" (map digitToWord (digits 42021)) )