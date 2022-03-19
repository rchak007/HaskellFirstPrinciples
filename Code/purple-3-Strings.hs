

-- 3.1 Printing strings

-- 3.2 A first look at types

-- Types are a way of categorizing values. There are several types for numbers, for example, 
-- depending on whether they are integers, fractional numbers, etc. 

-- Prelude> :type 'a'
-- 'a' :: Char

-- ****************************************************  :: symbol   **************************************************
-- the :: symbol is read as “has the type.”

-- Prelude> :type "Hello!"
-- "Hello!" :: [Char]

-- ****************************************************  Type Alias   **************************************************
-- String is a type alias, or type synonym, for a list of Char.


-- 3.3 Printing simple strings

-- Prelude> print "hello world!"
-- "hello world!"

-- Prelude> putStrLn "hello world!"
-- hello world!

-- ****************************************************  print vs putStrLn  putStr **************************************************

-- Prelude> :l print1.hs
-- [1 of 1] Compiling Print1
-- Ok, one module loaded.
-- *Print1> main
-- hello world!
-- *Print1>

-- Prelude> prompt may have changed to reflect the name of the module you loaded

-- ****************************************************  :module or :m **************************************************
-- :module or :m to unload the module and return to Prelude if you wish


-- ****************************************************  :set prompt **************************************************
-- Prelude> :set prompt "λ> "
-- λ> :r



-- see, main has the type IO () - for input/output.
-- special type, called IO, used when the result of running a program involves effects beyond evaluating a function or expression.


-- ****************************************************  do syntax **************************************************
-- do syntax is a special syntax that allows for sequencing actions.

-- Prelude> main
-- Count to four for me:
-- one, two, three, and four!


-- String concatenation




module Print3 where
myGreeting :: String
myGreeting = "hello" ++ " world!"
hello :: String
hello = "hello"
world :: [Char]     -- String
world = "world!"


-- 3.4 Top-level versus local definitions


-- ****************************************************  Top-level versus local definitions **************************************************
-- where and let clauses in Haskell introduce local bindings or declarations.
topLevelFunction :: Integer -> Integer
topLevelFunction x =
    x + woot + topLevelValue
    where woot :: Integer
          woot = 10
topLevelValue :: Integer
topLevelValue = 5


-- Exercises: Scope
-- area d = pi * (r * r)
-- r = d / 2

-- Ex - 4
area d = pi * (r * r)
    where r = d / 2


-- 3.5 Types of concatenation functions


-- the types of ++ and concat. 

-- **************************************************** Infix ++ /Prefix concat -  Infix operator in Prefix position (++) **************************************************

-- The ++ function is an infix operator. When we need to refer to an infix operator in a position that is not infix—such as when we are using it in a
-- prefix position or having it stand alone in order to query its type—we must put parentheses around it.

-- ++ has the type [a] -> [a] -> [a]
-- concat has the type [[a]] -> [a]

-- GHC 7.10 or newer, and you probably are, concat’s type signature will look different
-- Prelude> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- Prelude> :t concat
-- concat :: Foldable t => t [a] -> [a]
-- Prelude> :i (++)
-- (++) :: [a] -> [a] -> [a] 	-- Defined in ‘GHC.Base’
-- infixr 5 ++
-- Prelude> :i concat
-- concat :: Foldable t => t [a] -> [a] 	-- Defined in ‘Data.Foldable’

-- for now, please read Foldable t => t [a] as being [[a]].

-- type of concat says that it takes a list of lists as an input and returns a list.
-- A String is a list, a list of Char specifically, and concat can work on lists of strings or lists of lists of other things


-- Prelude> concat [[1, 2], [3, 4, 5], [6, 7]]
-- [1,2,3,4,5,6,7]
-- Prelude> concat ["Iowa", "Melman", "Django"]
-- "IowaMelmanDjango"


-- But what do these types mean?
-- (++) :: [a] -> [a] -> [a]
-- --       [1]   [2]    [3]

-- Everything after the :: is about our types, not our values.
-- The a inside the list type constructor [] is a type variable.

-- 1. Take an argument of type [a]. This type is a list of elements
-- of some type a. This function does not know what
-- type a is. It doesn’t need to know. In the context of the
-- program, the type of a will be known and made concrete
-- at some point.

-- 2. Take another argument of type [a], a list of elements
-- whose type we don’t know. Because the variables are the
-- same, they must be the same type throughout (a == a).

-- 3. Return a result of type [a].


func3 :: a -> a
func3 x = x

-- *Print3> func3 5
-- 5
-- *Print3> func3 "asdsds"
-- "asdsds"

-- func4 :: a -> b



-- **************************************************** Polymorphism **************************************************
-- The type variable a in [a] is polymorphic


-- Exercises: Syntax errors

-- Read the syntax of the following functions, and decide whether it will compile. Test them in your REPL, and try to fix the syntax
-- errors where they occur:

-- 1. ++ [1, 2, 3] [4, 5, 6]    -- ++ is infix so need parenthesis
-- 2. '<3' ++ ' Haskell'        -- should be double quota and not single
-- 3. concat ["<3", " Haskell"]  -- good


-- 3.6 Concatenation and scoping


-- print3flipped.hs
-- **************************************************** (++) right associative  **************************************************
-- In secondGreeting, using (++) as a prefix function forces us to shift some things around. Parenthesizing it that way emphasizes
-- its right associativity. 



-- **************************************************** where - local bindings  **************************************************
-- where clause creates local bindings for expressions that are not visible at the top level.


-- ************************************************* reading the compile error in Haskell - line and col #, actual problem, thing refer to that isnt visible *************
-- Let’s take a closer look at this error:
-- print3broken.hs:6:12: error:
--                [1][2]
-- Variable not in scope:
--                 [ 3 ]
-- greeting :: String
-- [ 4 ]

-- 1. The line the error occurs on: in this case, line 6.
-- 2. The column the error occurs on: column 12. Text on computers
-- is often described in terms of lines and columns.
-- These line and column numbers are about lines and
-- columns in your text file containing the source code.
-- 3. The actual problem: we refer to something not in scope,
-- that is, not visible to the printSecond function.
-- 4. The thing we refer to that isn’t visible, or in scope.


-- 3.7 More list functions

-- **************************************************** : operator  **************************************************************
-- The : operator, called cons, builds a list

-- Prelude> 'c' : "hris"
-- "chris"
-- Prelude> 'P' : ""
-- "P"

-- head returns the head or first element of a list:
-- Prelude> head "Papuchon"
-- 'P'
-- tail returns the list with the head chopped off:
-- Prelude> tail "Papuchon"
-- "apuchon"
-- take returns the specified number of elements from the list,
-- Prelude> take 1 "Papuchon"
-- "P"
-- Prelude> take 0 "Papuchon"
-- ""
-- Prelude> take 6 "Papuchon"
-- "Papuch"

-- drop returns the remainder of the list after the specified number of elements have been dropped:
-- Prelude> drop 4 "Papuchon"
-- "chon"
-- Prelude> drop 9001 "Papuchon"
-- ""
-- Prelude> drop 1 "Papuchon"
-- "apuchon"

-- already seen the ++ operator:
-- Prelude> "Papu" ++ "chon"
-- "Papuchon"
-- Prelude> "Papu" ++ ""
-- "Papu"

-- The infix operator, !!, returns the element that is in the specified position in the list.
-- Prelude> "Papuchon" !! 0
-- 'P'
-- Prelude> "Papuchon" !! 4
-- 'c'

-- **************************************************** unsafe  **************************************************************
-- while all of these are standard Prelude functions, many of them are considered unsafe. They are unsafe,
-- because they do not know how to handle an empty list. Instead, they throw out an error message, or exception,
-- Prelude> head ""
-- *** Exception: Prelude.head: empty list
-- Prelude> "" !! 4
-- *** Exception: Prelude.!!: index too large


-- 3.8 Chapter exercises
-- Reading syntax
-- 1. For the following lines of code, read the syntax carefully, and decide whether they are written correctly. Test them
-- in your REPL in order to check your work. Correct as many as you can:
-- a) concat [[1, 2, 3], [4, 5, 6]]       -- correct
-- b) ++ [1, 2, 3] [4, 5, 6]              -- Incorrect ++ needs parenthesis
-- c) (++) "hello" " world"               -- Correct
-- d) ["hello" ++ " world]                -- world is missig "
-- e) 4 !! "hello"                        -- reversed list has to come first
-- f) (!!) "hello" 4                      -- Correct
-- g) take "4 lovely"                     -- 4 should not be inside the double quotes
-- h) take 3 "awesome"                    -- Corrrect

-- other 2 exercises did in book itself.

-- Building functions
-- 1a - "Curry is awesome" ++ "!"
-- 1b - (!!) "Curry is awesome!" 4
-- 1c - drop 9 "Curry is awesome!" 


-- 2. Take each of the above, and rewrite it in a source file as a general function that could take different string inputs as
-- arguments but retain the same behavior. Use a variable as an argument to each of your (named) functions. If you’re
-- unsure how to do this, refresh your memory by looking at the waxOff exercise from the previous chapter and the
-- TopOrLocal module from this chapter.

dropX :: Int -> String -> String
dropX a s = drop a s

indexX :: Int -> String -> Char
indexX a s = (!!) s a

concatX :: String -> String -> String 
concatX a b = (++) a b

-- 3. Write a function of type String -> Char that returns the third character in a String. Remember to give the function
-- a name, and apply it to a variable, not a specific String, so that it could be reused for different String inputs, as
-- demonstrated. Feel free to name the function something else. Be sure to fill in both the type signature and the
-- function definition after the equals sign:

thirdLetter :: String -> Char
thirdLetter s = (!!) s 3

-- 4. Change the above function so that the string operated on is always the same and the variable represents the number
-- of the letter you want to return (you can use "Curry is awesome!" as your string input or a different string, if you
-- prefer):
letterIndex :: Int -> Char
letterIndex x = (!!) "Curry is awesome!" x


-- 5. Using the take and drop functions we looked at above, see if you can write a function called rvrs (an abbreviation of
-- “reverse,” used because there is already a function called reverse in Prelude, so if you give your function the same
-- name, you’ll get an error message). rvrs should take the string "Curry is awesome" and return the result "awesome is
-- Curry". This may not be the most lovely Haskell code you will ever write, but it is entirely possible using only what
-- we’ve learned so far. First, write it as a single function in a source file. This doesn’t need to, and shouldn’t, work
-- for reversing the words of any sentence. You’re expected only to slice and dice this particular string with take and drop.

-- reverse-ch3.hs  -- module Reverse

cur1 = "Curry is awesome"

rvrs :: String -> String
rvrs cur1 = do 
    let x1 = take 5 cur1     -- Curry
    let d1 = drop 5 cur1     -- "is awesome"
    let x3 = take 4 d1       -- "is "
    let d2 = drop 4 d1       -- "awesome"
    d2 ++ x3 ++ x1

rvrs1 :: String -> String
rvrs1 cur1 = do 
    let x1 = take 5 cur1     -- Curry
    let d1 = drop 5 cur1     -- "is awesome"
    let x3 = take 4 d1       -- "is "
    let d2 = drop 4 d1       -- "awesome"
    (drop 4 (drop 5 cur1)) ++ (take 4 (drop 5 cur1)) ++ (take 5 cur1)

rvrs2 :: String -> String
rvrs2 cur1 = do 
    let x1 = take 5 cur1     -- Curry
    let d1 = drop 5 cur1     -- "is awesome"
    let x3 = take 4 d1       -- "is "
    let d2 = drop 4 d1       -- "awesome"
    (drop 4 $ drop 5 cur1) ++ ( take 4 $ drop 5 cur1 ) ++ take 5 cur1



--module Print1 where
main :: IO ()

-- main = putStrLn "hello world!"

main = do
    putStrLn "Count to four for me:"
    putStr "one, two"
    putStr ", three, and"
    putStrLn " four!"
    putStrLn myGreeting
    putStrLn secondGreeting
          where secondGreeting =
                  concat [hello, " ", world]







