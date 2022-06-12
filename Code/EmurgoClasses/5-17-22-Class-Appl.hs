{-# LANGUAGE TypeApplications #-}
import Data.List
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Data.Either
import System.IO.Unsafe


-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/17/22 -- Tuesday
-----------------------------------------------------------------
-----------------------------------------------------------------

-- 17:43
-- what is the motivation for applicative

-- Pure and <*> operator.
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- functor is only for 1 argument.
-- Prelude> fmap negate [2,3,4]
-- [-2,-3,-4]

-- Prelude> fmap negate (Just 4)
-- Just (-4)
-- but you cant do (Just 3) + (Just 4)

-- ap :: Maybe (a -> b) -> Maybe a -> Maybe b
-- this pretty much looks like a function application if you remove the Maybe --
-- which is the 
-- Prelude> :t ($)
-- ($) :: (a ->b) -> a -> b
-- why do we have Function inside a Maybe.

-- Lazy vs Currying -- both are slightly diff.
add' x y z = x + y + z
-- add' = \x -> \y -> \z -> x + y + z
add'' = add' 1 2 
add''' = add' 1
-- *Main> add' 1 2 3
-- 6
-- *Main> add'' 3
-- 6
-- *Main> add''' 2 3
-- 6
-- *Main> :t add'
-- add' :: Num a => a -> a -> a -> a
-- *Main> :t add''
-- add'' :: Integer -> Integer
-- *Main> :t add'''
-- add''' :: Integer -> Integer -> Integer

-- *Main> :t add'
-- add' :: Num a => a -> a -> a -> a
-- *Main> :t (add' 1)
-- (add' 1) :: Num a => a -> a -> a
-- *Main> :t (add' 1 2)
-- (add' 1 2) :: Num a => a -> a
-- *Main> :t (add' 1 2 3)
-- (add' 1 2 3) :: Num a => a
-- *Main> (add' 1 2 3)
-- 6
-- *Main> add' 1

-- <interactive>:7:1: error:
--     • No instance for (Show (Integer -> Integer -> Integer))
--         arising from a use of ‘print’
--         (maybe you haven't applied a function to enough arguments?)
--     • In a stmt of an interactive GHCi command: print it
-- *Main> cannot show a Function 


-- this below is huge to understand
-- Prelude> :t fmap (+) (Just (2 :: Int))
-- fmap (+) (Just (2 :: Int)) :: Maybe (Int -> Int)
-- this is not wrong as you may think in first glance as + takes 2 arguments. But remember no function in Haskell really takes more than 1 argument. EVery function takes only 1 argument. 
-- Coz even the function that seems to be taking more than 1 argument are really just returning functions which take the rest of the argument.

-- since fmap makes func to apply inside the maybe.
-- lets say it was (+1) it would have done Maybe (1+2)  -- but now since we used (+) thats why now its only (+2) and waiting for another Int.

-- So becuase fmap does not change structure so we still have Maybe type but instead of Maybe Int now its Maybe (Int -> Int)

-- my own play
xMaybeFunc1 = fmap (+) (Just (2 :: Int))
xApp1 :: Maybe (Int -> Int) -> Int -> Int
xApp1 (Nothing) _ = 0
xApp1 (Just x) y = x y
-- *Main> xApp1 xMaybeFunc1 4
-- 6

-- Maybe is a type contructor. not a type on its own.
-- Maybe Int or Maybe (Int -> Int)

-- (+) :: Int -> (Int -> Int)
-- fmap :: (Int -> b) -> Maybe Int -> Maybe b
-- fmap :: (Int -> (Int -> Int)) -> Maybe Int -> Maybe (Int -> Int)

-- ap :: Maybe (a -> b) -> Maybe a -> Maybe b
-- ($) :: (a ->b) -> a -> b
-- ap is the $ in the Maybe world - analogy

-- in your Pure world you dont have error handling
-- in the Maybe world you somehow have failure propagation is not 100% full on error hanlding but have it free.
-- but to compromise with the syntax you have to use app function instead of $ operator.

-- ap (fmap (+) (Just 3)) (Just 4)
-- <*> is the applicative 
-- *Main> (<*>) xMaybeFunc1 (Just 4)
-- Just 6
-- *Main> (<*>) (fmap (+) (Just 3)) (Just 4)
-- Just 7

-- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
-- Just f <*> Just x = Just (f x)
-- _ <*> _  = Nothing

-- You simultaneously have to keep 2 diff interpreations of the functor and applicator in mind.
-- One happens to be wrapper interpretation - saying Maybe is a wrapper
-- one happens to be context interpretation - saying that it is context where failure prop is done and semantics are failure prop by default.

-- Prelude> (<*>) (fmap (+) (Just 3)) Nothing
-- Nothing
-- Prelude> (<*>) (fmap (+) (Nothing)) (Just 4)
-- Nothing
-- Prelude> (<*>) (fmap (+) (Nothing)) Nothing
-- Nothing
-- so the above 3 is ex of error prop.
-- So my null values are automatically propagated whenever something goes wrong.
-- 43:05

-- essense of applicative.  apply some function on the value.
-- so you have ( (+) $ 3) and then apply that on 4

appPlus = (+) $ 3
-- *Main> appPlus 4
-- 7

-- you apply the functon + on 3 and whatever becomes of the it , then again apply that on 4. 
-- convince yourself it is same thing as 3 + 4
-- that is what you lose, but its not heavy toll because you can say this:

-- Prelude> (Just (+) <*> Just 3) <*> Just 4
-- Just 7
-- intead of below
-- Prelude> ((+) $3) $ 4
-- 7


safeHeadMaybe' :: [a] -> Maybe a
safeHeadMaybe' [] = Nothing
safeHeadMaybe' (x:xs) = Just x

reg1 = ((+) $ head [3,5,7]) $ head [4,6,8]
reg2 = (Just (+) <*> safeHeadMaybe' [3,5,7]) <*> safeHeadMaybe' [4,6,8]
-- *Main> reg1
-- 7
-- *Main> reg2
-- Just 7
-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- takes function (+) inside the 1st Just and applied to 1st safeHeadMaybe' result which is Just 3. So at this point it will Just (+ 3) waiting for the next argument since + takes 2 arguments. since its Just f a it is Just (+) 3 at this point. 
-- Next it takes Just 4 from the 2nd safeHeadMaybe'. Since applicative it takes the new `f` that is ((+) 3) and applied to Just 4 to give Just 7

-- Error prop
-- *Main> (Just (+) <*> safeHeadMaybe' [3,5,7]) <*> safeHeadMaybe' []
-- Nothing

-- *Main> (Just (+) <*> safeHeadMaybe' []) <*> safeHeadMaybe' [4,6,8]
-- Nothing
-- *Main> (Just (+) <*> safeHeadMaybe' []) <*> safeHeadMaybe' []
-- Nothing

-- so we are not doing the Try Catch extra code to catch the errors. like in JS but we have good old fashioned functions.
-- you dont have overhead of knowing how it will work. There is no special syntax. 

-- applicative was not there in Haskell till 2009. Not its big part because its useful.

-- 52:00

-- Its easy to enter context of Maybe.
-- its not same of IO .. 

pureJ1 = (Just (+) <*> safeHeadMaybe' [3,5,7]) <*> safeHeadMaybe' [4,6,8]
-- *Main> pureJ1
-- Just 7

pure1 = (pure (+) <*> safeHeadMaybe' [3,5,7]) <*> safeHeadMaybe' [4,6,8]
-- *Main> pure1
-- Just 7
-- so you dont need to use Just here. you dont need to bother that you working in Maybe context in your function calls.
-- just say `pure` and since you are in the Maybe context its already inferred. So apporiate function will be called. In many cases there is no Function to call. And you are still good in those cases too. 
-- *Main> pure1
-- Just 7

-- *Main> pure (+1) <*> Just 10
-- Just 11

-- pureL1 = (pure (+) <*> [3,5,7]) <*> [4,6,8]
-- -- *Main> pureL1
-- -- [7,9,11,9,11,13,11,13,15]

-- no function call part --- so here the Function is also nothing but still there is error propagation.
-- Prelude> Nothing <*> Just 5 <*> Just 7
-- Nothing
-- Either also slightly like that. Becasue Left actslike that.
-- Prelude> Left 5 <*> Right 2 <*> Right 10
-- Left 5
-- Prelude> [] <*> [1,2,3] <*> [5,6,7]
-- []


-- class Functor f => Applicative f where
--   pure :: a -> f a                         -- pure
--   (<*>) :: f (a -> b) -> f a -> f b

-- in IO the same function is called return. in Monad class over tehre the same function is called return. Its called `pure` here and `return` in Monad. 
-- its like how in Monoid we called it mappend but in Semigroup it is <> 

-- idea behind return was --its nothign to do with Imperative return.
-- pure or return or sometimes its called Unit in some theoretic circles.

-- pure or return - dont return anywhere.... return just introduced a value... 


strC = pure (\x y -> x ++ y) <*> getLine <*> getLine
-- *Main> strC
-- Rafa
-- Nadal
-- "RafaNadal"
-- NOte - it only introduces the value but GHCI makes it print. the double quotes tells that i did not ask to print but GHCI is doing it. 

-- context changed , syntax remains the same , the symantics change.  (Maybe vs IO now)

-- 1:00:00


-- how powerful these higher order type classes can be.. also called constructor classes when it was new - 20/30 yrs ago.. 
-- Applicative is not regular type class or interface coz its not really interfacing on types... its giving you an interface to the type class. 
-- Functor is also a Constructor clas.. so also higher order than your normal Num, Eq, even Monoid classes.
-- so abstracting not over type but over a type constructor.

strC2 = pure (\x y -> x ++ y) <*> Just ("one") <*> (Just "two")
-- *Main> strC2
-- Just "onetwo"
-- basically moving from IO context to Maybe Context. How small difference to easily switch like that
-- in Maybe context you dont have Input Output but have error handling but in getLine you had IO but no error handling.. 


-- printStrC2 = pure (\x y -> x ++ y) <*> Just ("one") <*> (Just "two") 

-- {-# LANGUAGE TypeApplications #-}
nErr1 = pure (\x y -> x + y) <*> fmap (read @Int) getLine <*> fmap (read @Int) getLine
-- *Main> nErr1
-- 5
-- 7
-- 12
-- *Main> nErr1
-- one
-- 3
-- *** Exception: Prelude.read: no parse    -- no error handling

-- 1:05:00 - 
-- now onto Lists
-- *Main> :t sequenceA
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

-- *Main> :t sequenceA @[] @IO
-- sequenceA @[] @IO :: [IO a] -> IO [a]
-- takes a list of IO actions.
-- in the haskell world IO action is also a value. 
-- ex.
xGt = getLine -- this does not do anythign because i have not evaluated getLine. i have just assigned to a value called xGt. Now `xGt` is a binding which is bound to the value getLine. Now getLine is not a funtion its just a value which has type IO String.
yLGt = [xGt, xGt, xGt, xGt, xGt]
-- sequecneA wil enable me to get continous input 
-- Prelude> :t sequenceA @[] @IO @String
-- sequenceA @[] @IO @String :: [IO String] -> IO [String]
-- *Main> sequenceA yLGt
-- one
-- two
-- three
-- four
-- fice^?ve
-- ["one","two","three","four","fice\DELve"]

-- GHCI scipt of Irfan - ~/.ghci
-- :set prompt "> "
-- :set -XTypeApplications
-- :set -XExplicitForAll
-- :set -XMonadComprehensions
-- import Data.Char


-- task
-- prompt user for 5 numbers
-- print the sum of all numbers user entered.

-- main = replicate 5 (fmap read getLine)   -- this still wont do anything yet - compile will fail as its type is not same.
-- *Main> :t (replicate 5 (fmap read getLine))
-- (replicate 5 (fmap read getLine)) :: Read b => [IO b]
-- *Main> :t main
-- main :: IO ()
-- main is IO () but this one is [IO b]   -- list of IO Int/String etc.



-- sequenceA $ replicate 5 (fmap read getLine)    -- this is now list of numbers in IO context

-- main = sum <*> (sequenceA $ replicate 5 (fmap read getLine))  - does not work.. since it expects Function in IO context but this `sum` is a naked function.
-- *Main> :t sum
-- sum :: (Foldable t, Num a) => t a -> a
-- *Main> sum [1,2,3]
-- 6

-- so we need to do is say pure
-- main = pure sum <*> (sequenceA $ replicate 5 (fmap read getLine))
-- *Main> main
-- 1
-- 2
-- 3
-- 4
-- 5
-- 15
-- this worked because of GHCI.. 

-- now it does not print ---- cause rememeber that the above kept it in IO context and we need to print it. 
--  ghc myExercises.hs 
-- [1 of 1] Compiling Main             ( myExercises.hs, myExercises.o )
-- Linking myExercises ...
-- /nix/store/v8imx1nvyz0hgvx9cbcmh6gp4ngw3ffj-binutils-2.35.1/bin/ld.gold: warning: /nix/store/9bh3986bpragfjmr32gay8p95k91q4gy-glibc-2.33-47/lib/crt1.o: unknown program property type 0xc0008002 in .note.gnu.property section
--  ./myExercises
-- 1
-- 2
-- 3
-- 4
-- 5
--  

-- student asked it is the deriving show .. Irfan said thats to convert something to a String. Thats different from Printing it. 
-- putStrLn is the impure action.
-- show is pure. 

-- *Main> :t pure sum <*> (sequenceA $ replicate 5 (fmap read getLine))
-- pure sum <*> (sequenceA $ replicate 5 (fmap read getLine)) :: (Num b, Read b) => IO b

-- main = pure sum <*> (sequenceA $ replicate 5 (fmap read getLine)) >>= \x -> putStrLn.show $  x

-- Now it prints
--  ghc myExercises.hs 
-- [1 of 1] Compiling Main             ( myExercises.hs, myExercises.o )
-- Linking myExercises ...
-- /nix/store/v8imx1nvyz0hgvx9cbcmh6gp4ngw3ffj-binutils-2.35.1/bin/ld.gold: warning: /nix/store/9bh3986bpragfjmr32gay8p95k91q4gy-glibc-2.33-47/lib/crt1.o: unknown program property type 0xc0008002 in .note.gnu.property section
--  ./myExercises
-- 1
-- 2
-- 3
-- 4
-- 5
-- 15

--1:20:00

-- *Main> main
-- 1
-- 45
-- 67
-- 23
-- 9
-- the sum is 145
-- there is no way to get out of the Context but we can only print it.

-- there is still 1 thing i wont be able to do with applicative.

-- you want to have the goodies of IO and error handling.. we will see later. 

-- you cannot get the 5 in replicate 5 from the user.
-- you cannot do sequenceA dependent calculations. 

-- cannot make calculation dependent on prior calculation. 
-- later we see Monad - one more level powerful. 
-- 
-- thats another problem with applicative where you can easily walkd into a  nested context where things dont behave as you want them to.
-- Monad solves this problem. it will take a function that has to return an IO context but it will compose it so you dont nest it. do it wont blindly fmap it but will fmap it and collapse it down to the simple IO context.


-- convert IO int to IO string 
xIOInt :: IO Int
xIOInt = fmap read getLine
showXIoInt =  xIOInt >>= \x -> putStrLn (show x )
-- *Main> showXIoInt
-- 5
-- 5
-- *Main> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- so basically the bind >>= takes IO Int [this is the `m a` part] 
-- and then show takes the Int and converts to String and PutStrLn takes the String converts to IO. 
-- this whole part is finally (a -> m b) [basically you can take the `a` and do whatever you want but in the end you need to bring it back to `IO b` which putStrLn does ]
-- so bottomline if you have an IO `x` you bind it and take the `x` to show and do putStrLn then you have printed it. 




problemAppl = pure (\x y _-> y ++ x) <*> getLine <*> getLine <*> putStrLn "hello"
-- *Main> problemAppl 
-- one
-- two
-- hello
-- "twoone"       --- because the pure had only 2 argument since 3rd we threw is away - did not apply
-- and also double quotes means its ghci

-- pure (\y _ -> y ++ "rafa") <*> getLine <*> putStrLn "hello"
-- pure (_ -> "nadal" ++ "rafa") <*> putStrLn "hello"
-- so here putStrLn "hello" first prints as this will be processed and then the IO () is returned. but if not applied to the function. So actual final value remains the IO "nadalrafa" 


problemAppPutStr = pure (\x y _-> putStrLn . fmap toUpper $ (y ++ x)) <*> getLine <*> getLine <*> putStrLn "hello"
-- *Main> problemAppPutStr
-- one 
-- two
-- hello

str1 = fmap toUpper $ "nadalrafa"
pStr1 = putStrLn str1
-- *Main> :t str1
-- str1 :: [Char]
-- *Main> :t pStr1lem
-- pStr1 :: IO ()
-- *Main> :t ((\x y _-> putStrLn . fmap toUpper $ (y ++ x)))
-- ((\x y _-> putStrLn . fmap toUpper $ (y ++ x)))
--   :: [Char] -> [Char] -> p -> IO ()
-- since we are taking a function that return IO () (due to the putStrLn), then since its Pure it further wraps with IO to go into the  "putStrLn "hello"" - thats why we end up with IO (IO ())
-- the pure wraps with IO.
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- your `b` is already IO () from the naked function basically. 


-- it does not print because this whole thing becomes a Nested IO
-- *Main> :t problemAppPutStr
-- problemAppPutStr :: IO (IO ())
-- IO of IO so it does not to 

xRafa = ("rafa" ++ "nadal")
-- *Main> :t fmap toUpper $ ("rafa" ++ "nadal")
-- fmap toUpper $ ("rafa" ++ "nadal") :: [Char]
-- *Main> fmap toUpper $ ("rafa" ++ "nadal")
-- "RAFANADAL"
xUpRf = fmap toUpper $ xRafa
-- *Main> xUpRf
-- "RAFANADAL"
putxUpRf = putStrLn.fmap toUpper $ xRafa     -- but this is not in IO context.. so ignore..
-- *Main> putxUpRf
-- RAFANADAL


-- corr1 = pure (\x y _-> fmap toUpper $ (y ++ x)) <*> getLine <*> getLine  >>= \x -> putStrLn (show x)



-- 1:31:00

listApp1 = pure (\x y -> (x,y) ) <*> [1,2,3]  <*> [5,6,7]
-- *Main> listApp1
-- [(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7)]
-- Did you expect [1,5], [2,6], [3,7]  ??

-- order is important
listApp1b = pure (\x y -> (x,y) ) <*> [5,6,7] <*> [1,2,3]  
-- *Main> listApp1b
-- [(5,1),(5,2),(5,3),(6,1),(6,2),(6,3),(7,1),(7,2),(7,3)]


-- In computer programming, a nondeterministic algorithm is an algorithm that, even for the same input, can exhibit different behaviors on different runs, as opposed to a deterministic algorithm. There are several ways an algorithm may behave differently from run to run

-- non-deterministic computation
-- [2,3,4]

first = ["Anlberto", "Bryan", "Carlos"]
second = ["Black", "White","Brown"]
listApp2 = pure (\x y -> "Hello. Mr " ++ x ++ " " ++ y ) <*> first  <*> second
-- *Main> listApp2
-- ["Hello. Mr Anlberto Black","Hello. Mr Anlberto White","Hello. Mr Anlberto Brown","Hello. Mr Bryan Black","Hello. Mr Bryan White","Hello. Mr Bryan Brown","Hello. Mr Carlos Black","Hello. Mr Carlos White","Hello. Mr Carlos Brown"]

-- is also like Cartesian product - its non determistic computation and its like a nested for loop.

listApp3 = pure (\x y z-> "Hello. Mr " ++ x ++ " " ++ y ++ " " ++ z) <*> first  <*> second <*> ["one", "two"]
-- now 18 possibilities - 3 x 3 x 2


-- Home work.
-- List of  functions
-- (<*>) :: [a->b] -> [a] -> [b]
-- (f:fs) <*> (x:xs) = ....   -- hint will involve fmap
-- every such f should operate on whole list first and then next function will operate.. --- what is supposed to happen. 

-- Homework List of Function.
appL :: [a->b] -> [a] -> [b]
appL [] _ = []
appL _ [] = []
appL (f:fs) (x:xs)  = fmap f (x:xs) ++ appL fs (x:xs) 

funcAppL1 :: String -> Int
funcAppL1 s = length s

funcAppL2 :: String -> Int 
funcAppL2 s = 2 * (length s)

funcList = [funcAppL1, funcAppL2]
appLlistA = ["the", "World", "goes", "round", "and", "round"]

-- *Main> appL funcList appLlistA
-- [3,5,4,5,3,5,6,10,8,10,6,10]





listApp4 = pure (\x y -> (x,y) ) <*> [6,7] <*> [1,2,3]  
-- *Main> listApp4
-- [(6,1),(6,2),(6,3),(7,1),(7,2),(7,3)]

listApp5 = pure (\x y -> (x,y) ) <*> [] <*> [1,2,3]
-- *Main> listApp5
-- []

-- Null value of list also acts like a surrage error handling. 


-- listApp1 = pure (\x y -> (x,y) ) <*> [1,2,3]  <*> [5,6,7]
-- if you want this to behave like [(1,5), (2,6), (3,7)] then we need a newtype

newtype NewList a = NewList { getNewList :: [a]}

-- import Control.Applicative
-- 
-- *Main> import Control.Applicative
-- *Main Control.Applicative> :i ZipList
-- type ZipList :: * -> *
-- newtype ZipList a = ZipList {getZipList :: [a]}
-- ZipList is exactly like what i have here as newtype

listApp6 = pure (\x y -> (x,y) ) <*> ZipList [1,2,3]  <*> ZipList [5,6,7]
-- *Main Control.Applicative> listApp6
-- ZipList {getZipList = [(1,5),(2,6),(3,7)]}

-- *Main> pure (+) <*> ZipList [1,2,3] <*> ZipList [5,6,7]
-- ZipList {getZipList = [6,8,10]}


zip1 = zip [2,3,4] [6,7,8]
-- *Main Control.Applicative> zip1
-- [(2,6),(3,7),(4,8)]
-- *Main> :t zip
-- zip :: [a] -> [b] -> [(a, b)]
-- thats what ZipList is doing.
-- Ziplist is defined in termds of zip function internally

-- 1:54:00

-- 1 element less
listApp7 = pure (\x y -> (x,y) ) <*> ZipList [1,2,3]  <*> ZipList [5,6]
-- *Main Control.Applicative> listApp7
-- ZipList {getZipList = [(1,5),(2,6)]}

getListApp7 = getZipList listApp7     -- this we already know
-- *Main Control.Applicative> getListApp7
-- [(1,5),(2,6)]


-- List and ZipList
-- pure :: a -> [a]
-- pure a = [a]
-- (<*>) :: [a -> b] -> [a] -> [b]
-- [] <*> (a:as) = []
-- (f:fs) <*> (a:as) = fmap f (a:as) ++ fs <*> (a:as)

ap'' :: [a -> b] -> [a] -> [b]
ap'' [] as = []
-- ap (f:fs) (as) = fmap f (as) ++ (fs <*> (as))
ap'' (f:fs) (as) = fmap f (as) ++ (ap'' fs (as))

-- `ap` already definifed in base GHC
-- *Main> :i ap
-- ap :: Monad m => m (a -> b) -> m a -> m b   -- Defined in ‘GHC.Base’

listApp8 = pure (\x y -> (x,y) ) <*> [2,3,4] <*> [6,7,8] 
-- *Main Control.Applicative> listApp8
-- [(2,6),(2,7),(2,8),(3,6),(3,7),(3,8),(4,6),(4,7),(4,8)]

listApp9 = pure (\x y -> (x,y) ) `ap''` [2,3,4] `ap''` [6,7,8] 
-- *Main Control.Applicative> listApp9
-- [(2,6),(2,7),(2,8),(3,6),(3,7),(3,8),(4,6),(4,7),(4,8)]

-- pure :: a -> ZipList a
--   will use repeat function
-- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b 
--   will use ZipList b

-- reference - 
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b




-- Question and answer!
-- Chakravarti Raghavan, [May 21, 2022 at 9:37:22 AM]:
-- i wanted to ask a question about terminology 
-- Like when we have fmap (+1) (Just 3).  
-- Then i can say that the Just here is a Maybe Functor right? I know Maybe can have instances of Eq, Applicative, etc.. but in this context of fmap, the Just is a Maybe Functor? would that be right to say terminology wise?

-- -- *Main> (<*>) (fmap (+) (Just 3)) (Just 4)
-- -- Just 7
-- and then in this example the part of Just 3 is a Maybe Functor as its in the fmap context? of course that becomes a Maybe Int -> Int after fmap application.
-- and the 2nd Just 4 acts as Maybe Applicator cause the context there is Applicative?

-- irfan ali, [May 21, 2022 at 9:55:13 AM]:
-- the way i see it Just is a constructor and Maybe is the Functor. the way Functor is declared only a type constructor of kind * -> * could be a Functor. Just is a function. its not a type or a type constructor. it lives at the value universe. implementations or instances of Functor just like any other typeclass are inhabitants of the type universe. i am not sure if this answers the question but if its just about terminology thats what i would say.

-- when we referred to context in class it could be phrased like this. a value like Just 3 is 3 in the Maybe context. Maybe happens to be a Functor so we get to use fmap on Just 3. Maybe also happens to be an applicative so we can use (<*>) in that. fmap does not change the context and neither does (<*>). context is the type constructor. the way we define fmap and (<*>) and so on determines what behaviour we get in it. for IO we get interaction. for Maybe we get failure propagation. for Either we get error propagation etc.

-- hope thats clear!
-- basic funda -- Maybe is a type constructor thats why we can do kind of Maybe. 
-- Just is a value constructor. We say Just 3 to create the and Just lives in the value universe. 



-- Own pratice 
func1IntEx :: Int -> Int
func1IntEx i = i + 10

justFunc1IntEx = Just (func1IntEx) <*> Just 10
-- *Main> justFunc1IntEx
-- Just 20


-- funcM1M2 :: (Monad m, Monad n) => m a -> (a -> n b) -> n b 
right1 :: Either String Int
right1 = (Right 25) >>= \x -> Right (x + (fromRight 0 (funcMaybeEither (Just 25))))

funcMaybeEither :: Maybe a -> Either String a
funcMaybeEither (Just a) = Right a
funcMaybeEither Nothing = Left "error"

-- *Main> right1
-- Right 50

funcStr :: String -> String -> String
funcStr s1 s2 = s1 ++ s2 
funcIOStr :: IO (String -> String -> String)
-- funcIOStr s1 =  ( return (funcStr ) :: IO (String -> String -> String) )
-- funcIOStr  = return (funcStr )     -- works
funcIOStr  = pure (funcStr ) 

appIOStr :: IO String
appIOStr = funcIOStr <*> getLine <*> getLine
-- *Main> appIOStr
-- first
-- second
-- "firstsecond"


main = do 
    pure sum <*> (sequenceA $ replicate 5 (fmap read getLine)) >>= \x -> putStrLn $ "the sum is " ++  show x
    putStrLn (show strC2)
    putStrLn ( "Result = " ++ (show (right1 )))
    problemAppl

-- sequenceA --- takes [ IO Int, IO Int, IO Int, IO Int, IO Int] and makes it an IO [Int, Int, Int, Int, Int] so 
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- now the bind takes the [Int, Int, Int, Int, Int] from IO context and applies Sum on it. and leaves it in the IO Context of course. the show makes the sum a string and putStrLn outputs to screen.







 