{-# LANGUAGE ScopedTypeVariables #-}


module MapFilter where

import Test.QuickCheck
import Text.Show.Functions 
import Test.QuickCheck.Poly

import Test.QuickCheck.Function


import Data.List.NonEmpty as N
    ( length, tail, head, NonEmpty((:|)) )
import Data.Monoid hiding ((<>))
import Data.List as L

import Control.Monad

import Data.Coerce







-- lets take a list 
shrList = [1,4567,33,444,28, 31]
predicateL x = x > 100     -- Filter predicate
filShrList = filter predicateL shrList
-- *MonoidsSemigroups> filShrList 
-- [4567,444]
mapFunc1 :: Int -> Int
mapFunc1 x = x + 100
mapShrFilList1 = map mapFunc1 (filter predicateL shrList)
-- *MonoidsSemigroups> mapShrFilList1
-- [4667,544]
mapShrFilList2 :: [Int]
mapShrFilList2 = filter predicateL (map mapFunc1 shrList)
-- *MonoidsSemigroups> mapShrFilList2
-- [101,4667,133,544,128,131]

funcToList = map mapFunc1 shrList
-- *MonoidsSemigroups> funcToList 
-- [101,4667,133,544,128,131]

shrAnswer = mapShrFilList1 == mapShrFilList2
-- map f (filter p xs) = filter p (map f xs)     --- so this statement is not true






prop_MapFilter f p xs =
        map f (filter p xs) == filter p (map f xs)
------ this one was only for import Test.QuickCheck and no concretized so its all empty and it gives pass 
----- also had to add import Text.Show.Functions to even show the pass. otherwise we had no instance of show error
-- *Addition> quickCheck prop_MapFilter
-- +++ OK, passed 100 tests.
-- *Addition> verboseCheck prop_MapFilter
-- Passed:   
-- <function>
-- <function>
-- [(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]

-- +++ OK, passed 100 tests.


prop_MapFilter' f p (xs :: [A]) =               
    -- import Test.QuickCheck.Poly to enable the signature :: [A] to make it polymorpic
    map f (filter p xs) == filter p (map f xs)
-- *Addition> quickCheck prop_MapFilter'
-- *** Failed! Falsified (after 3 tests and 1 shrink):     
-- <function>
-- <function>
-- [1]

prop_MapFilter'' f p (xs :: [A]) =
------ here we added some stuff whenFail to print more info so there is more meaning
  whenFail (print (table f xs)) $
    whenFail (print (table p ( xs ++ map f xs))) $
        map f (filter p xs) == filter p (map f xs)
table h xs = [ (x, h x) | x <- xs ]
-- *Addition> quickCheck prop_MapFilter''
-- *** Failed! Falsified (after 4 tests):                  
-- <function>
-- <function>
-- [2]                    --- interpreset as first list        
-- [(2,3)]                --- function takes it to this
-- [(2,True),(3,False)]   --- predicates


--- below is simple verification for the Failure
xsL :: [Int]
xsL = [2]
funcToTup :: Int -> (Int, Int)
funcToTup i = (i, i+1)
--funcToList' [x] = [(x,x+1)]
-- funcToList' (x:xs) =   (x,x+1) : funcToList' xs


pred' :: Int -> Bool
pred' x 
    | x==2 = True
    | otherwise = False

filtL = filter pred' xsL

left1 = map funcToTup (filter pred' xsL)
-- *Addition> left1
-- [(2,3)]

mapL = map funcToTup xsL
-- *Addition> mapL
-- [(2,3)]
-- right1 = filter pred' (map funcToTup xsL)    --- so now pred' cant act on [(2,3)] as out pred' acts on just Int



prop_MapFilter''' (Fun _ f) (Fun _ p) (xs :: [A]) =               
    map f (filter p xs) == filter p (map f xs)
{- Just showing the implementation of data Fun
data Fun a b = Fun (...) (a->b)
instance (Show a, Show b) => Show (Fun a b)
-}
-- *Addition> quickCheck prop_MapFilter'''
-- *** Failed! Falsified (after 4 tests and 17 shrinks):    
-- {_->1}
-- {3->True, _->False}
-- [3]

-- prop_HaskellML p = 
--     p "Haskell 98" ==> p "Standard ML"

prop_HaskellML (Fun _ p)  = 
    p "Haskell 98" ==> p "Standard ML"
-- *Addition> quickCheck prop_HaskellML
-- *** Failed! Falsified (after 2 tests and 122 shrinks):     
-- {"Haskell 98"->True, _->False}
-- *Addition> quickCheck prop_HaskellML
-- *** Failed! Falsified (after 1 test and 134 shrinks):     
-- {"Standard ML"->False, _->True}


newtype Comp a =
    Comp { unComp :: (a -> a) } 
-- instance Semigroup (Comp a ) where 
--   Comp {unComp=f} <> Comp {unComp=g} = Comp f

instance Show (Comp a) where      -- can show a function so you should intialize something random so it does not complain of lack of Show
    show f = "Unicorns!!"

instance (Semigroup a) => Semigroup (Comp a) where
    (Comp fx) <> (Comp fy) = Comp (fx . fy)

-- Arbitrary instance which has function  
-- solution from - https://stackoverflow.com/questions/47849407/coarbitrary-in-haskell
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- Test.QuickCheck.arbitrary
        return (Comp f)

-- -- Arbitrary instance which has function -- can also do genComp like how i did and call that.. 
-- instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
--     arbitrary = genComp

genComp :: (Arbitrary a, CoArbitrary a) => Gen (Comp a)
genComp = do 
    f <- Test.QuickCheck.arbitrary 
    return (Comp f)

type CompSumIntTest =
     Sum Int -> Comp (Sum Int)  -> Comp (Sum Int) -> Comp (Sum Int) -> Bool

compAssoc :: (Semigroup a, Eq a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = (unComp (a <> (b <> c)) $ v) == (unComp ((a <> b) <> c) $ v)



compAssocSumInt = quickCheck ( compAssoc :: CompSumIntTest)