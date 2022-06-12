
{-# LANGUAGE TypeApplications #-}
import Data.List
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.State

import System.IO.Unsafe



safeHeadMaybe :: [a] -> Maybe a
safeHeadMaybe [] = Nothing
safeHeadMaybe (x:xs) = Just x

-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/20/22 -- Tuesday
-----------------------------------------------------------------
-----------------------------------------------------------------
--10:35

--pure is to convert a normal function to applicative function. but not just a function. Type of Pure is polymorpic.
-- so the `a` can be anything. 

-- Prelude> :t pure
-- pure :: Applicative f => a -> f a
-- f is applictive but `a` can be anything.
-- Prelude> pure 7 :: Maybe Int
-- Just 7
-- Prelude> pure 7 :: [] Int
-- [7]

-- Prelude> pure head :: Maybe ([a]->a)

-- <interactive>:6:1: error:
--     • No instance for (Show ([a0] -> a0)) arising from a use of ‘print’
-- this is not an error - its not able to print it.

-- Prelude> :t pure head :: Maybe ([a]->a)
-- pure head :: Maybe ([a]->a) :: Maybe ([a] -> a)
-- if you are able to get the type like this it means we are good. Its just cant print

-- Prelude> :t pure head :: [] ([a]->a)
-- pure head :: [] ([a]->a) :: [[a] -> a]
-- Function in list context

--16:55 

-- Prelude> :t replicate 3 getLine
-- replicate 3 getLine :: [IO String]   -- its a list of IO which is no use. i want IO of something not list of IO.

-- so we use sequenceA
-- Prelude> :t sequenceA $ replicate 3 getLine
-- sequenceA $ replicate 3 getLine :: IO [String]

-- now i have IO context

-- instead of Bind i can use pure.
-- if i use Bind---
xSeq3Get = sequenceA $ replicate 3 getLine :: IO [String]
-- bind will get the [String] out and we can do whatever we want but later restore to IO context in the end
-- *Main> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b


-- Prelude> replicate 3 1
-- [1,1,1]
-- Prelude> :t replicate
-- replicate :: Int -> a -> [a]
-- Replicate just does 3 getLines and puts it in a list.
-- so now i have [IO String, IO String, IO String]
-- sequenceA - makes it IO [String, String, String]
-- Bind then extract just the [String, String, String] into x and then show x converts all to string and putStrLn then prints and return IO () so we maintaing the IO context we started with ( IO [String, String, String] )


bindXSeq3Get = xSeq3Get >>= \x -> putStrLn (show x)
-- *Main> bindXSeq3Get 
-- Rafa
-- The
-- Goat
-- ["Rafa","The","Goat"]
-- *Main> toUpper 'a'
-- 'A'
-- *Main> fmap toUpper "rafa"
-- "RAFA"


-- instead of just show it converts to Upper with fmap.
bindXSeq3GetUpper = xSeq3Get >>= \x -> putStrLn (show (fmap (fmap toUpper) x))
-- *Main> bindXSeq3GetUpper 
-- rafa
-- the
-- goat
-- ["RAFA","THE","GOAT"]

-- now doing with `pure` applicative function on it instead of inside bind. 
pureGetUpper = pure (\x y -> (x ++ y)) <*> getLine <*> getLine
-- *Main> pureSeq3GetUpper 
-- rafa
-- goat
-- "rafagoat"    -- but this is printing due to GHCI

bindPureGetUpper = pure (\x y -> fmap (fmap toUpper) (x : y : [])) <*> getLine <*> getLine >>= \x -> putStrLn (show x)
-- 2 fmaps because the 1st fmap lifts into the List and next to Lift into the String. Also this is not using sequenceA like before. 

-- *Main> bindPureSeq3GetUpper
-- rafa
-- nadal
-- ["RAFA","NADAL"]

bindPureGetUpperSeq = pure (\ss -> fmap (fmap toUpper) (ss)) <*> sequenceA (replicate 3 getLine) >>= \x -> putStrLn (show x)

-- *Main> bindPureGetUpperSeq
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]


-- we dont need even the lamda to get the input from seq/getLine etc.. 
bindPureGetUpperSeq' = pure (fmap (fmap toUpper)) <*> sequenceA (replicate 3 getLine) >>= \x -> putStrLn (show x)
-- *Main> bindPureGetUpperSeq'
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]


-- since its only 1 value being used we can just do it with fmap itself
fmapPureGetUpperSeq = fmap ((fmap (fmap toUpper))) (sequenceA (replicate 3 getLine) )
-- *Main> fmapPureGetUpperSeq
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]
-- so you are lifting IO, then list, then string - 3 fmaps.

-- applicative library itself provides a synonym for fmap which is <$>

-- *Main> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- *Main> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

fmapPureGetUpperSeq1 =  (fmap (fmap toUpper)) <$> (sequenceA (replicate 3 getLine) )
-- *Main> fmapPureGetUpperSeq1
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]

fmapPureGetUpperSeq2 = ((fmap toUpper) <$>)  <$> (sequenceA (replicate 3 getLine) )

-- *Main> fmapPureGetUpperSeq2
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]

fmapPureGetUpperSeq3 = ((toUpper <$>) <$>)  <$> (sequenceA (replicate 3 getLine) )

-- *Main> fmapPureGetUpperSeq3
-- rafa
-- nadal
-- goat
-- ["RAFA","NADAL","GOAT"]

-- 23:45


-- applicative 2 things that are issues:
-- #1 - it cannot make dependent calculation.
-- #2 - also i cant use a funtion that return IO since i already have IO context so the funtion would make IO of IO. and i am stuck

dep1  = (+) <$> (fmap read getLine) <*> (fmap read getLine)
-- *Main> dep1
-- 3
-- 4
-- 7
-- i cant have 2nd fmap be dependent on the earlier getLine input. 
-- so its only sequential operation.

--- break down - both fmap gives me IO Int 
-- (+) <$> (IO Int)  <*>  (IO Int) 
-- fmap (+) (IO Int) <*> (IO Int)     --- rewriting in prefix
-- IO (+ Int) <*> (IO Int)      -- now we apply function + Int to Int inside the IO Context.
-- IO (Int + Int)
-- thats why 3 and 4 input gives me 7. But then its the ghci printing the 7 becuase result is IO 7
-- *Main> :t dep1
-- dep1 :: IO Integer
-- 




-- <*> applicative is also seems to be called as Fighter.

-- So Monad solves this problem. Monad gives full blown control which the applicative is not providing. Applicative is more general and less powerful.

-- only in IO impure code we have notion of running something sequential. nowhere else in haskell sequencing is relevant. 

-- just notation below..
-- (\x y -> ....) <$> (x) <*> (y)   
-- you cannot make `y` dependent on `x`. Cant influence `y` with `x`.
-- Monad will provide that solution. 

-- Issue # 2 - 
-- (\x y -> putStrLn (x ++ y)) <$> getLine <*> getLine
-- here putStrLn return IO () and then already in applicative i needto stay in IO context due to getLine . So i am stuck in nesdted IO. 

-- *Main> :t (\x y -> putStrLn (x ++ y)) <$> getLine <*> getLine
-- (\x y -> putStrLn (x ++ y)) <$> getLine <*> getLine :: IO (IO ())
-- it will not produce error but it will also not produce any results. 

-- *Main> (\x y -> putStrLn (x ++ y)) <$> getLine <*> getLine
-- 1
-- 2

-- the <*> forces you to be in the context due to f b . here our putStrLn in our function `(a -> b)` takes you from String -> IO (). And out `f` is IO too as getLine is IO String.
-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- and there are plenty of function that return IO. So that is a problem.

-- 41:15

fmapApp1 = (+) <$> safeHeadMaybe [2,3,4] <*> safeHeadMaybe [7,8,9]
-- *Main> fmapApp1
-- Just 9

fmapApp2 = pure (+) <*> safeHeadMaybe [2,3,4] <*> safeHeadMaybe [7,8,9]
-- *Main> fmapApp2
-- Just 9

-- basically for 1st value like just 1 value i can just do fmap. its same as doing applicative with pure. 

-- 43:26

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)

-- will this work? - it should work but you get a nested Just like like nested IO earlier
nestedJust1 = safeDiv <$> safeHeadMaybe [24,3,4] <*> safeHeadMaybe [7,8,9]
-- *Main> nestedJust1 
-- Just (Just 3)

-- that is the problem Monad is going to solve.
-- just a way to make dependent computations but in IO feels sequential but other places it gives you full control over function application in a context.
-- you can have 2 values and apply function to them in a context how applicative allows you to do, but then you can also have a value abd another value dependent and do somethign in that context too.

ret :: a -> Maybe a
ret a = Just a 

-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b 
-- fmap :: Maybe a ->  (a ->  b) -> Maybe b   -- order is flipped
-- app ::  Maybe a ->  Maybe (a -> b) ->  Maybe b    -- -- order is flipped  

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

-- in bind the function depends on actual value `a` inside the context. it now introduces dependency. 
-- it produced a new Maybe b and chains it It collapses the nested Maybe b to produce a Mayb b.
-- this in combination with ret will be able to bypass the app and fmap both of them.


bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing k = Nothing
bind (Just a) k = k a 

-- world :: [a] -> [a] -> Maybe a
world :: [Int] -> [Int] -> Maybe Int
world xs ys = safeHeadMaybe xs `bind` (\x -> safeHeadMaybe ys `bind` (\y -> safeDiv x y))

-- *Main> world [24,65,23] [8,4,45]
-- Just 3
-- breakdown
--   world [24,65,23] [8,4,45] = 
--        safeHeadMaybe [24,65,23] `bind` (\x -> safeHeadMaybe [8,4,45] `bind` (\y -> safeDiv x y))
--        Just 24 `bind` (\x -> Just 8 `bind` (\y -> safeDiv x y))
--        x = 24, y = 8 
--        safeDiv 24 8
--        Just 3      -- this satisfies the type signature.. since we started with safeHeadMaybe [24,65,23] which is Just 24   -- so the Context Maybe was established. and we ended with Maybe context. 






-- 1:02:00
bindMaybe1 = Just 3 >>= (\x -> Just 5 >>= \y -> Just (x + y))
-- *Main> bindMaybe1
-- Just 8
-- the Bind after \y returns `m b` which is maybe b and same thing with after \x and same thing overall - a Maybe b is returned. 

bindMaybe2 = Just 3 >>= (\x -> Just 5 >>= \y -> Just "Hello")
-- *Main> bindMaybe2
-- Just "Hello"

-- 

-- Prelude> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- Prelude> :t (>>=) @Maybe
-- (>>=) @Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b


-- infix notation
-- Prelude> ((2+3) + 4) + 5  -- brackets not necessary here actually
-- 14

-- Prelude> ((+) ((+)  ((+) 2 3 ) 4) 5)    -- prefix
-- 14

-- thats why we used `bind` as infix operator its easier than prefix.

-- so this return and bind are both functions fo the Monad type class. 

-- Prelude> :i Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

world' :: [Int] -> [Int] -> Maybe Int
-- world xs ys = safeHeadMaybe xs `bind` (\x -> safeHeadMaybe ys `bind` (\y -> safeDiv x y))
world' xs ys = safeHeadMaybe xs >>= (\x -> safeHeadMaybe ys >>= (\y -> safeDiv x y))

-- Changing to do notation
doworld' :: [Int] -> [Int] -> Maybe Int
-- world xs ys = safeHeadMaybe xs `bind` (\x -> safeHeadMaybe ys `bind` (\y -> safeDiv x y))
doworld' xs ys =  do 
             x <- safeHeadMaybe xs 
             y <- safeHeadMaybe ys 
             safeDiv x y



-- *Main> world' [24,65,23] [8,4,45]
-- Just 3

-- list of employees or students 
type Name' = String
type Learner = (Name', Int)
type Class = [Learner]
learners = [ ("Mark", 23) , ("Damian", 34), ("Todd", 56)]
-- find:: Name -> Class -> Learner   -- but it can give error if learnring is not there.
find' :: Name' -> Class -> Maybe Learner     -- maybe will handle error.
-- find' = undefined
-- do for homework - try to make insert find, 
-- find name learners >>= \l -> ..... return a Maybe ...

find' n [] = Nothing
find' n (c : cs) =  if ( n == (fst c)) 
                   then (Just c) 
                   else  (find' n cs) 
insertLearner :: Maybe Name' -> Maybe Int -> Class
insertLearner Nothing _ = learners
insertLearner _ Nothing = learners
insertLearner (Just n) (Just a) = (n,a) : learners

findName = getLine >>= \x -> putStrLn ( show (find' x learners ))
getInsertLearner = getLine >>= \x -> getLine >>= \y -> putStrLn ( show (insertLearner (Just x) (Just $ read y)))
-- *Main> findName
-- Damian
-- Just ("Damian",34)
-- *Main> findName
-- asdas
-- Nothing

-- *Main> getInsertLearner 
-- Rafa
-- 36
-- [("Rafa",36),("Mark",23),("Damian",34),("Todd",56)]



tup1 = ("rafa", 36)
-- *Main> fst tup1
-- "rafa"

-- 1:16:00 -- we will talk about List Monad
-- Monad is a type constructor like a list or Maybe.. its like applicative or Functor .. its not a type. but type constructor.

-- find all prime numbers below a number.
-- primes 12
-- [2,3,5,7,11]

-- 12  
-- 12 /11 , 12/ 10, 12 /9... 12/6 = 2.
-- 11
-- 11/ 10, 9, 8,7,6,5,4,3,2,

primesMy1 :: Int -> [Maybe Int]
primesMy1 i = if (i<2) then [Nothing]
              else 
              if (i==2) then ([Just 2])
              else if (rem1 i (i-1) == True && ((i-1) >= 2) ) 
                   then ( Just i : primesMy1 (i-1))
                   else if (rem1 i (i-1) == True && ((i-1) < 2) ) 
                        then [Nothing]
                        else (primesMy1 (i-1))

rem1 :: Int -> Int -> Bool 
rem1 x y = if ((rem x y ) == 0 )
           then False 
           else if ((y-1) == 1) then True
                 else rem1 x (y-1)


-- trying to use list and not with Maybe since i can use [] instead of Nothing
primesMy1' :: Int -> [Int]
primesMy1' i = if (i<2) then []      -- if user runs something lowert than 2
              else 
              if (i==2) then ([2])   -- stop at 2
              else if (rem1' i (i-1) == True && ((i-1) >= 2) ) 
                   then ( i : primesMy1' (i-1))
                   else if (rem1' i (i-1) == True && ((i-1) < 2) ) 
                        then []
                        else (primesMy1' (i-1))

rem1' :: Int -> Int -> Bool 
rem1' x y = if ((rem x y ) == 0 )
           then if x ==2 then True else False 
           else if ((y-1) == 1) then True
                 else rem1' x (y-1)
-- *Main> primesMy1' 7
-- [7,5,3,2]
monadPrime a = [a] >>= \x -> primesMy1' (x-1)
-- *Main> monadPrime 7
-- [7,5,3,2]

monadPrime2 i = [2 .. (i-1)] >>= \x -> if (rem1' x (x-1)) == True 
                           then  [x] 
                           else []


-- 1:18:00


-- Prelude> pure (+) <*> [1,2,3,4] <*> [5,6,7]
-- [6,7,8,7,8,9,8,9,10,9,10,11]
-- 1:25:00



multiples :: Int -> [Int]
multiples x = [2..x] >>= (\a -> [2..x] >>= (\b -> [a*b]))
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

-- *Main> :t (>>=) @[]
-- (>>=) @[] :: [a] -> (a -> [b]) -> [b]
-- *Main> :t (>>=) @[] @Int @Int
-- (>>=) @[] @Int @Int :: [Int] -> (Int -> [Int]) -> [Int]

-- bascially the m here is List.  and (a -> m b) is the last lambda takes you from b -> List (a*b) . of course for us `a` and `a*b` are both Int. and m is List. Because we retain the List. Now the outer lamda (\b -> [a*b]) means it return `m b` that is a List. so now the inner \a -> ... is List. so its a -> List b too. Only thing is function is taking its input from earlier `m a`

-- *Main> multiples 7
-- [4,6,8,10,12,14,6,9,12,15,18,21,8,12,16,20,24,28,10,15,20,25,30,35,12,18,24,30,36,42,14,21,28,35,42,49]

-- mutiples with just applicative
appMult x = pure (\x y -> x*y) <*> [2..x] <*> [2..x]
-- *Main> appMult 7
-- [4,6,8,10,12,14,6,9,12,15,18,21,8,12,16,20,24,28,10,15,20,25,30,35,12,18,24,30,36,42,14,21,28,35,42,49]

-- mulitples with fmap  -- you stuck with function inside List after 1st fmap. So i need <*> 
-- thats why we said fmap work on 1 value.
fmapMult x =   (fmap (\a b-> a*b) [2..x]) <*> ([2..x])



-- *Main> length $ multiples 7
-- 36
-- the mechanism here to find prime numebrs is that we multiplied from 2 ... (x-1 ) into each. 

-- do i need to explicitly put them inside a list , do i have way to inject a value into a monadic context?
multiples' :: Int -> [Int]
multiples' x = [2..x] >>= (\a -> [2..x] >>= (\b -> return $ a*b))
-- *Main> multiples' 7
-- [4,6,8,10,12,14,6,9,12,15,18,21,8,12,16,20,24,28,10,15,20,25,30,35,12,18,24,30,36,42,14,21,28,35,42,49]

-- this above multiples was suggested by student..
-- changing to do notation
dOmultiples' :: Int -> [Int]
dOmultiples' x = do 
                 a <- [2..x]  
                 b <- [2..x] 
                 return $ a*b



-- initally Irfan wanted to do with factors to get prime numbers.
factors :: Int -> [Int]
factors x = 
    [2..(x-1)] >>= \a -> 
    if mod x a == 0 then [a] else []
-- *Main> factors 24
-- [2,3,4,6,8,12,24]

-- bind looks like fmap
-- map :: [a] -> (a -> b) -> [b]   -- flipped version
-- >>= :: [a] -> ( a -> [b]) -> [b]
-- diff is in the function [b] is the diff

-- *Main> map (^2) [2,3,4]
-- [4,9,16]
-- *Main> map (\x -> [x^2]) [2,3,4]
-- [[4],[9],[16]]
-- same problem i have with map-- i get nested list

-- there is a very simple function to get a list out of nested list. 
-- *Main> concat $ map (\x -> [x^2]) [2,3,4]
-- [4,9,16]

-- so Bind is nothing but Concat map also called Flat map; 
-- *Main> [2,3,4] >>= (\x -> [x^2])
-- [4,9,16]


-- *Main> map (\a -> if mod 24 a == 0 then [a] else []) [2..24]
-- [[2],[3],[4],[],[6],[],[8],[],[],[],[12],[],[],[],[],[],[],[],[],[],[],[],[24]]

-- *Main> concat $ map (\a -> if mod 24 a == 0 then [a] else []) [2..24]
-- [2,3,4,6,8,12,24]

factorPrime :: Int -> [Int]
factorPrime x = 
    [2..x] >>= \a -> 
    if (null (factors a)) then [a] else [] 

-- *Main> factorPrime 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


-- 1:46:00

-- How list monad help you in writing these things. 
-- now its like in imperative speak (although its not true here) you can sequence your computaton. Does this look like FOR loop for you. 
-- Introduce DO notation -- whereever i see \a -> i change
dofactorPrime :: Int -> [Int]
dofactorPrime x = do 
    a <- [2..x]
    if (null (dofactors a)) then [a] else [] 

dofactors :: Int -> [Int]
dofactors x = do
    a <- [2..(x-1)] 
    if mod x a == 0 then [a] else []

-- *Main> dofactorPrime 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


-- there is also another typeclass that does this automatically for you. that has a funtion called Guard and the list implementation tells you that it takes a boolean & it does this in Monadic context. You can look into it it makes the syntax even more pleasing. 

-- so this works in Monadic context so it toatlly different than imperative lanuguage. I also get Error propagation.



-- Changing to do notation
listDoworld' :: [Int] -> [Int] -> [Int]
-- world xs ys = safeHeadMaybe xs `bind` (\x -> safeHeadMaybe ys `bind` (\y -> safeDiv x y))
listDoworld' xs ys =  do 
             x <- xs 
             y <- ys 
             return $ x * y

-- *Main> listDoworld' [2,3] [7,12]
-- [14,24,21,36]
-- we change the doWorld' from Maybe Int to [Int]
-- barely anything changed.. of course somethings have to change to change the context.. bnut the meaning, semantics and exefcution everythign is differnet.. 
-- Monad in popular literature is described as a programmable syntax, or programmable semi colon. 

-- this below also works -- we put semi colon
smListDoworld' :: [Int] -> [Int] -> [Int]
-- world xs ys = safeHeadMaybe xs `bind` (\x -> safeHeadMaybe ys `bind` (\y -> safeDiv x y))
smListDoworld' xs ys =  do {
             x <- xs;
             y <- ys; 
             return $ x * y
            }
-- *Main> smListDoworld' [2,3] [7,12]
-- [14,24,21,36]

-- implement fmap in monadic way
fmap' h mx = mx >>= \x -> return $ h x
-- *Main> fmap' (^2) [2..10]
-- [4,9,16,25,36,49,64,81,100]
-- So fmap is purely implementabl just using bind. You dont even need to have knowledge of that particular monad.
-- if you look at fmap' definityion i dont know for which Monad this will be used. it will work for any monad.

-- *Main> fmap' negate (Just 7)
-- Just (-7)
-- *Main> fmap' read getLine :: IO Int
-- 5
-- 5

-- *Main> fmap' ((^2) . read) getLine :: IO Int
-- 7
-- 49

-- defining applicative with Monadic context

-- what Irfan gave
app mf mx = mf >>= \f -> fmap' f mx 
-- *Main> pure (*) <*> fmap' read getLine <*> fmap' read getLine
-- 2
-- 7
-- 14

-- now using our `app`
-- *Main> pure (*) `app` fmap' read getLine `app` fmap' read getLine
-- 2
-- 7
-- 14

-- i expanded to write everything here itself
app'' mf mx = mf >>= (\f ->  (mx  >>= (\a -> return $ f a) ))
-- first bind will extract the Function from  f (a->b) which here is `mf` and the 2nd bind extract the `a` from `f a`. Then `f a` is applying naked function on the separated `a`. The `return` will put this back in the monadic context. 

-- Prelude> fmap (*2) [1,2]
-- [2,4]

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b


-- Prelude> pure (*) <*> [1] <*> [1,2]
-- [1,2]

-- exMonadApp = pure (*) >>= \f -> [1,2] >>= 


-- Prelude> fmap (*2) [1,2]
-- [2,4]
-- Prelude> pure (*2) <*> [1,2]
-- [2,4]
-- Prelude> [1,2] >>= (\a -> [(*2) a])
-- [2,4]
-- above you are doing List but when you do getLine you get IO wrapper which is the Functor/Applicative or Monadic contexts.
-- instaed you would do 2 getLines you get 2 numbers and then multiply

-- [1,2] >>= (\a -> [(*2) a])

-- ioInt1 :: IO Int
ioInt1 = (fmap' read (getLine)) :: IO Int

-- Prelude> pure (+2) <*> (fmap read getLine)
-- 5
-- 7
--    pure (+2)  will put it in IO function context. f (a -> b). here f is IO and a->b is the (+2)
-- then fmap will use naked function read to go into IO String which is getLine. and convert that to IO Int. 
-- then the applicative will apply (+2) with 5 from getLine and still keep it in the IO context. so we have IO Int in the end.

-- *Main> :t (pure (+2) <*> (fmap read getLine))
-- (pure (+2) <*> (fmap read getLine)) :: (Num b, Read b) => IO b


exMonadApp1 = (pure (*)) `app''` ioInt1 `app''` ioInt1
-- *Main> exMonadApp1 
-- 4
-- 5
-- 20

-- instance Functor IO   -- just to note. so i can always fmap into a getLine


xx' = (pure (*2) ) >>= (\f ->  (fmap read getLine) >>= \a -> return $ f a)
-- *Main> xx'
-- 5
-- 10

-- 1:57:57 - 
-- somtimes Functor syntax is very terse.

getNumberF = (read <$> getLine ) :: IO Int   -- this looks better to use
getNumberM = getLine >>= (return.(read @Int))

--Applicative and Fucntors can very easily be composed together.

-- *Main>  fmap negate [2,3,4]
-- [-2,-3,-4]

-- *Main> fmap (fmap negate) [Just 2, Just 3, Just 4]
-- [Just (-2),Just (-3),Just (-4)]

-- Applicative - can be composed  very easily.
-- there is no general easy way to compose Monads. thats another problem there with Monads. They are so powerful there is no general way to compose.
-- Monad transformers - solutons are there but solutions are half adhoc. you can have a transforme composed woth maybe with anythigng.. or some other monda with some other monad .. but we cannot have 2 geneasrl monad that composes to make a third monad. 

-- next class State and reader stuff.. monad are quitr powerful on absracting out boiler plate ...
-- idea there is how do you manage state in imperative language.. you have some global variable.. 
-- haskell does not have mutability.
-- the only way to do mutability is through function and the only way to do state change is through function.
-- and a language should not be restrictive like that - so state monad provides that... you kind of abstracting away these kind of things by making it a type of its own , till now we saw Maybe and list monad which are polymoripic.. but we will also see State monads which are polymorphic on the functions rather than list or Maybe. ..
-- there is also reader monad.. read only.. not writing to. 


-- project - -
-- if you use Monad or transformers bonus points.. 


-- some transactins to be sent out.. 
-- like a pays b. b pays c. 
-- we create structure called Block .. maybe look at pyChain project. 

-- ghci> import Data.Hashable
-- ghci> hash "foo"
-- 60853164


