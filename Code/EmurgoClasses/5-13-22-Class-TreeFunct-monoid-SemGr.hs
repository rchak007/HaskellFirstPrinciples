

{-# LANGUAGE TypeApplications #-}
import Data.List
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.State

import System.IO.Unsafe


-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/13/22 --
-----------------------------------------------------------------
-----------------------------------------------------------------

safeHeadMaybe :: [a] -> Maybe a
safeHeadMaybe [] = Nothing
safeHeadMaybe (x:xs) = Just x
-- *Main> fmap negate $ safeHeadMaybe [7,8,9]
-- Just (-7)

-- 52:00

-- *Main> fmap toUpper getChar
-- a'A'
-- getChar just gets a character as input

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where 
-- mapTree :: (a -> b) -> Tree a -> Tree Bool
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Node l a r) = Node (fmap h l) ( h a) (fmap h r)

tree1 = Node (Leaf 4) 7 (Node (Leaf 3) 8 (Leaf 2))

-- *Main> :t tree1
-- tree1 :: Tree Integer

-- *Main> fmap negate tree1
-- Node (Leaf (-4)) (-7) (Node (Leaf (-3)) (-8) (Leaf (-2)))
-- *Main> fmap (*2) tree1
-- Node (Leaf 8) 14 (Node (Leaf 6) 16 (Leaf 4))

-- once you declare a Functor/Monoid instances for your type basically you get benefit of all those 
-- once you make a member of typeclass then you get all the exclusive benefits of which that typeclass provides.
-- you dont have to make it Monoid but then you but there are so many functions associate with that thing. 

-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Functor.html
-- shows all the benefist you get.
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html
-- same thing with Monad.

-- *Main> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- *Main> ['a'..'z']
-- "abcdefghijklmnopqrstuvwxyz"
-- *Main> :i Enum
-- type Enum :: * -> Constraint
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
-- example - if you define fromEnum etc you get these.


-- *Main> :t (<$)
-- (<$) :: Functor f => a -> f b -> f a
-- *Main> 23 <$ tree1
-- Node (Leaf 23) 23 (Node (Leaf 23) 23 (Leaf 23))
-- because we wrote a Functor instance for tree i can do this.

-- 1:08:00 -- Monoids
-- folds is supposed to be associated with monoids.

-- Identity
-- Subrtaction does not have identiy -
-- coz x - 0 = 0 but 0 - x = -x
-- division also does not have identity
-- And identity is True, Or identity is False.


-- Prelude> :i Monoid
-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a

-- Definition: Monoid
-- a type which has an operator `(@) :: a -> a -> a` and an identity `i :: a` which follows these laws:
-- 1. forall x. i @ x = x
-- 2. forall x. x @ i = x
-- 3. forall x y z. (x @ y) @ z = x @ (y @ z)

-- somethign different you might not seen before.
-- it's a typeclass but not a method but has actual value -
-- i dont want a function but just value of that type.
-- (++) []
-- (+)  0
-- (*)  1
-- (&&) True
-- (||) False

-- simplest monoid is list.
-- semigroup is more relaxed kind of structure.. Monoid needs the identity.
-- subtraction does not be semigroup either since no associativity...
-- there are things that dont have Identity.
-- ++ and mappend are same.

-- Prelude> mempty
-- ()
-- Prelude> mempty :: [a]      if we say for List then it gives []
-- []

-- Prelude> "one" <> "two"
-- "onetwo"
-- Prelude> getLine <> getLine
-- one
-- two
-- "onetwo"
-- Prelude> :t (getLine <> getLine)
-- (getLine <> getLine) :: IO String


-- Prelude> foldr (<>) (return "") (replicate 5 getLine)
-- rafa
-- nadal
-- the 
-- goat
-- tennis
-- "rafanadalthe goattennis"
-- coz foldr has to operate inside foldable to fold - replicate 5 getLine will give us 5 getLines in a list.
-- Prelude> foldr (<>) (return "")  ([getLine, getLine,getLine, getLine, getLine])
-- 1
-- 2
-- 3
-- 4
-- 5
-- "12345"

-- Since Maybe is also a Monad i can use return in this mappend too with another Just.
-- Prelude> (Just "r") <> (Just "x")
-- Just "rx"
-- Prelude> (return "r") <> (Just "x")
-- Just "rx"



-- Prelude> :t (foldr (<>) (return "") (replicate 5 getLine))
-- (foldr (<>) (return "") (replicate 5 getLine)) :: IO [Char]

-- 1:40:00
-- talks about how Int and Bool have more than 1 instances - 
-- coz what is mappend Int?? could be + or * .

-- type NewInt = Int
-- instance Semigroup NewInt where.. 
-- this wont work since type is just alias. Compiler will still treat as Int.

-- data NewInt vs newtype NewInt -- since its Unary constructor we can go with newtype.
-- New type 1 contructor NewInt and one field Int. 
-- NewInt is a wrapper for Int. Its a box.

-- 2:13:00
-- whenever you can use newtype use newtype. So if its unary use newtype.
-- both Data and newtype -- overhead for new type is not there- wrapper will be optimized away. 
-- Sum and Product types are the standard already defined. 

-- note- Semigroup instance for Monoid Maybe  Maybe already has Nothing as an empty value. 

-- Bool as newtype Any and newtype All

-- newtype Endo a      -- its a function a -> a
-- *Main> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
-- *Main> appEndo computation "Haskell"
-- "Hello, Haskell!"

-- appEndo unWraps computation and applies Hello with Haskell.
-- Endo is wrapping. AppEndo takes out the Endo. 
-- 2nd one will on the -- it will do Composition.
-- (a -> a) <> (a -> a)
square' x = x * (x :: Int)
-- *Main> :t square'
-- square' :: Int -> Int
endoSq = Endo square' <> Endo square'  -- its like Pipeline
-- *Main> :t endoSq
-- endoSq :: Endo Integer
appEndo3 = appEndo (endoSq) 3
-- *Main> appEndo3
-- 81

-- its square of 3 and then square of all that.

endo2 = Endo ((Sum 1) <> ) <> Endo (<> Sum 2)
endo1 = Endo ((Sum 1) <> ) 
sm2 = Sum 5
sm3 = appEndo endo1 sm2
sm4 = appEndo endo2 sm2
-- *Main Data.Monoid> sm3
-- Sum {getSum = 6}
-- Endo is basically function waiting for `a` to be applied. 
-- appEndo unwraps endo1 and applies to sum2.
-- in above square' example you can also chain it. Or same as sm4
-- *Main Data.Monoid> sm4
-- Sum {getSum = 8}

-- 2:25:00 
-- Applicative 
-- Haskell history -- 
-- Before there was only Functor and Monad. also Monoid was there.
-- functor did some calculation in some context and Monad made it general. So you can chain your calculations. With Functor thats a bit difficult.
-- Monad lets you chain very well. IO is monad. 
-- Monad was seen as natural progression from a Functor.
-- if you define bind operation on a Functor and return operation then it becomes Monad. We already saw some examples.
-- in general fmap is the thing whcih most data type had and its immediatly clear what type is.

-- *Main> fmap (^2) $ Just 3
-- Just 9

-- 2:30:00


world :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
world h (Just x) (Just y) = Just (h x y)
world _ _ _ = Nothing

-- *Main> world (+) (Just 3) (Just 4)
-- Just 7

-- what if i want to add 3 numbers, or multiply or 4 arguments?
-- so i want a general way to apply on any numbers of Maybe's

-- fmap is good, it looks like it gives you lot of things but nor really. does not give everything. 
-- you get ability to run some function inside Maybe context but only 1 value.. what if i need for host of values. THats what you are looking at Applicatives.
-- Idea with applicative is in the name.. it applies.. 
-- 

-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- its type is Maybe.
-- *Main> :t (fmap negate (Just 4))
-- (fmap negate (Just 4)) :: Num b => Maybe b


-- Prelude> :t fmap (+) (Just 4)
-- fmap (+) (Just 4) :: Num a => Maybe (a -> a)

-- Prelude> :t fmap (+) (Just (4 :: Int))
-- fmap (+) (Just (4 :: Int)) :: Maybe (Int -> Int)

-- you are now in difficult situation with function stuck in a Maybe. you dont think as stuck but like Function is introduced into Maybe context.. now you need to find a way to apply the function in Maybe context.
-- if i can do that. 
-- Maybe (Int -> Int) -> Maybe Int -> Maybe Int
-- applicative has a way for that...

-- re-writing world in applicative way - our own custom applicative 
ap' :: Maybe (a->b) -> Maybe a -> Maybe b 
ap' (Just f) (Just x) = Just (f x)
ap' _ _ = Nothing

-- *Main> ap' (fmap (+) (Just 4)) (Just 3)
-- Just 7

-- ($) ::       (a->b) ->     a   ->   b 
-- ap' :: Maybe (a->b) -> Maybe a -> Maybe b 
-- you are now in maybe world and you get error handling for free.
-- also called lifting. 
-- also not just simple +, it allow any sort of calculations.

-- *Main> (+) `fmap` (Just 4) `ap'` (Just 3) 
-- Just 7


ap4 = (\w x y z -> w + x + y + z) `fmap` (Just 4) `ap'` (Just 3) `ap'` (Just 12) `ap'` (Just 19)
-- *Main> ap4
-- Just 38
-- here whole Lambda func is being applied by fmap into Just 4. so its still needs another 3. 
-- first ap' applies it into Just 3 to become a Function expecting abother 2 arguments, then next ap' applies into Just 12 which becomes a funtion expecting 1 more value and then final ap' applies into Just 19 
level1 = Just (\x y z -> 4 + x + y + z) -- since w is applied.
level2 = Just (\y z -> 4 + 3 + y + z)
level3 = Just (\z -> 4 + 3 + 12 + z)
level4 = Just (4 + 3 + 12 + 19)
-- its like doing below but in Maybe world
plus4 = (\w x y z -> w + x + y + z) 4 3 12 19
-- *Main> plus4
-- 38

apN = (\w x y z -> w + x + y + z) `fmap` (Just 4) `ap'` Nothing `ap'` (Just 12) `ap'` (Just 19)
-- *Main> apN
-- Nothing

aps1 = (\x y -> x ++ y) `fmap` (Just "Hello, ") `ap'` (Just "World")
-- *Main> aps1
-- Just "Hello, World"

-- using the standard then i do this into any other structures too
eitherApp1 = (\x y -> x + y) `fmap` (Right 10) <*> (Right 20)
-- *Main> eitherApp1
-- Right 30