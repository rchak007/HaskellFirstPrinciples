inc :: Num a => a -> a
inc = (+1)

-- *Main> inc 3
-- 4

-- *Main> :t (+)
-- (+) :: Num a => a -> a -> a
-- *Main> :t (+1)
-- (+1) :: Num a => a -> a

three = inc . inc . inc $ 0
-- different syntax, same thindividedBy
three' = (inc . inc . inc) 0

threePlus :: Num a => a -> a
threePlus x = inc . inc . inc $ x
-- *Main> threePlus 4
-- 7

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n =
    n
incTimes times n =
    1 + (incTimes (times - 1) n)

-- *Main> incTimes 5 4
-- 9



applyTimes :: (Eq a, Num a) =>
     a -> (b -> b) -> b -> b                       -- reason we have 'a' is that is number of times we want recursion - so  thats a Num 
                                                   -- 'b' is there cause i can write recursion for any function not just +1 althugh for oour example we will write for now (+1)
                                                   -- but i could have written Concat for a string for example.
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n


incRafaTimes' :: (Eq n, Num n) => n -> [Char] -> [Char]        --- here we demonstrate that above  function f can be anything and here it was (++ " Rafa ")
incRafaTimes' times a = applyTimes times (++ " Rafa ") a

-- *Main> incRafaTimes' 10 " great "
-- " great  Rafa  Rafa  Rafa  Rafa  Rafa  Rafa  Rafa  Rafa  Rafa  Rafa "


--

-- Intermission: Exercise
-- Write out the evaluation of the following.
-- applyTimes 5 (+1) 5 = 
-- *Main> applyTimes 5 (+1) 5
-- 10

--   (+1) (applyTimes 4 (+1) 5)
-- *Main> (+1) (applyTimes 4 (+1) 5)
-- 10

--   (+1) ( (+1) applyTimes 3 (+1) 5) )
-- *Main> (+1) ( (+1) (  applyTimes 3 (+1) 5) )
-- 10


-- *Main> (+1) ( (+1) ((+1) (applyTimes 2 (+1) 5)) ) 
-- 10




fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
    fibonacci (x - 1) + fibonacci (x - 2)

-- fibonacci 6 = fibonacci (6-1) + fibonacci (6-2) 
-- fibonacci 6 = fibonacci 5 + fibonacci 4
-- fibonacci 6 = ( fibonacci 4 + fibonacci 3 ) + ( fibonacci 3 + fibonacci 2)
-- fibonacci 6 = ( (fibonacci 3 + fibonacci 2) + ( fibonacci 2 + fibonacci 1) ) + ( (fibonacci 2 + fibonacci 1)  + ( fibonacci 1 + fibonacci 0) )

-- fibonacci 6 = ( ( (fibonacci 2 + fibonacci 1)  + ( fibonacci 1 + fibonacci 0) + ( (fibonacci 1 + fibonacci 0)  + 1) ) + ( ( (fibonacci 1 + fibonacci 0)  + 1)  + ( 1 + 0) )
-- fibonacci 6 = ( ( (fibonacci 1 + fibonacci 0) + 1)  + ( 1 + 0) + ( (1 + 0)  + 1) ) + ( ( (1 + 0)  + 1)  + ( 1 + 0) )
-- fibonacci 6 = 8 



-- 8.6 Chapter exercises

-- Review of types
func :: [a] -> [a] -> [a]
func x y = x ++ y

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- *Main> flip cattyConny "rafa" "great" 
-- "great mrow rafa"

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny
-- *Main> flip cattyConny "rafa" "great"
-- "great mrow rafa"

appedCatty :: String -> String
appedCatty = cattyConny "woops"
-- *Main> appedCatty "shhh"
-- "woops mrow shhh"

frappe :: String -> String
frappe = flippy "haha"
-- *Main> frappe "woops"
-- "woops mrow haha"


-- *Main> appedCatty "woohoo!"
-- "woops mrow woohoo!"

-- *Main> frappe "1"
-- "1 mrow haha"

-- frappe (appedCatty "2")  = frappe "woops mrow 2" = flip cattyConny "haha" "woops mrow 2" = "woops mrow 2 mrow haha"
-- *Main> frappe (appedCatty "2")
-- "woops mrow 2 mrow haha"


-- appedCatty (frappe "blue") = appedCatty (flippy "haha" "blue") = appedCatty ( flip cattyConny "haha" "blue") = 
--	appedCatty "blue mrow haha" = cattyConny "woops" "blue mrow haha" = "woops mrow blue mrow haha"

-- *Main> appedCatty (frappe "blue")
-- "woops mrow blue mrow haha"

-- cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- cattyConny (frappe "pink") (cattyConny "green" (cattyConny "woops" "blue"))
-- cattyConny (frappe "pink") (cattyConny "green" ("woops mrow blue"))
-- cattyConny (frappe "pink") ("green mrow woops mrow blue")
-- cattyConny (flippy "haha" "pink") ("green mrow woops mrow blue")
-- cattyConny (flip cattyConny "haha" "pink") ("green mrow woops mrow blue")
-- cattyConny ("pink mrow haha" ) ("green mrow woops mrow blue")
-- "pink mrow haha mrow green mrow woops mrow blue"
-- *Main> cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- "pink mrow haha mrow green mrow woops mrow blue"


-- cattyConny (flippy "Pugs" "are") "awesome"
-- cattyConny (flip cattyConny "Pugs" "are") "awesome"
-- cattyConny ("are mrown Pugs") "awesome"
-- "are mrown Pugs mrow awesome"

-- Recursion

-- Ex 1 
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise =
            go (n - d) d (count + 1)

-- write out steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.

-- dividedBy 15 2
-- go 15 2 0
--   go 13 2 1
--     go 11 2 2
--       go 9 2 3
--         go 7 2 4
--           go 5 2 5
--             go 3 2 6
--               go 1 2 7
--                 (7, 1)

-- *Main> dividedBy 15 2
-- (7,1)

-- Ex 2 
-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument. 
-- So if n is 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a) => a -> a.

recurAdd :: (Ord a, Eq a, Num a) => a -> a

-- 1st step need to find the base case when to stop recursion. 
-- So here it is 1. but we need to handle 0 too. What about negative? 

recurAdd 0 = 0   -- to handle 0 being passed
recurAdd 1 = 1
recurAdd n = if n < 0 then 0 else n + recurAdd (n-1)    -- this works
-- recurAdd n = if n < 0 then 0   -- guess you need to supply else too otherwise gives compile error
-- recurAdd n = n + recurAdd (n-1)

-- 3. Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a.

recurMult :: (Integral a) => a -> a -> a
recurMult n 0 = 0
recurMult n 1 = n
recurMult n times = n + recurMult n (times -1)

-- 5 3 
-- 5 + 5 2 
-- 5 + 5 + 5 1
-- 5 + 5 + 5 = 15















