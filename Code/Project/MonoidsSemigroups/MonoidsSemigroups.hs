
{-# LANGUAGE FlexibleContexts #-}

module MonoidsSemigroups where

-- import Data.Semigroup as S     -- Since Data.Monoid is already imported seems this one is not needed or creating conflict
import Data.Semigroup (stimes)   -- only import stimes
import Data.List.NonEmpty as N
    ( length, tail, head, NonEmpty((:|)) )
import Data.Monoid hiding ((<>))
import Data.List as L



import Test.QuickCheck
-- import Test.QuickCheck.Poly as QP

import Text.Show.Functions 
import Test.QuickCheck.Poly
import Control.Monad

import Data.Coerce

-- import Data.List.NonEmpty as N

-- Run as
-- stack ghc --package QuickCheck -- purple-15-monoid-semigroup.hs


-- 15.1 Monoids and semigroups

-- finer points of the Haskell community has been its propensity for recognizing abstract patterns in code that 
-- have well-defined, lawful representations in mathematics.
-- algebra, by which we mean one or more operations and the set they operate over.

-- Some you may have heard of, such as functor and monad. Some, such as monoid and the humble semigroup, may seem new to you.
-- and it’s important to master them before we can do some of the exciting stuff that’s coming.

-- This chapter will include:
-- • Algebras!
-- • Laws!
-- • Monoids!
-- • Semigroups!


-- 15.2 What we mean by algebra

-- Algebra generally refers to one of the most important fields of mathematics. In this usage, it means the study of mathematical
-- symbols and the rules governing their manipulation. It is differentiated from arithmetic by its use of abstractions such as variables. 
-- By the use of variables, we’re saying we don’t care much what value will be put into that slot. 
-- We care about the rules of how to manipulate this thing without reference to its particular value.

-- an algebra refers to some operations and the set they operate over. Here again, we care less about the particulars of the values or 
-- data we’re working with and more about the general rules of their use.
 -- ********************************  Type Classes **********************************************************
-- In Haskell, these algebras can be implemented with type classes— the type class defines the set of operations.
-- When we talk about operations over a set, the set is the type the operations are for. 

-- One of those algebras we use is monoid. If you’re a working programmer, you’ve probably had monoidal patterns in your code already, 
--    perhaps without realizing it.




-- 15.3 Monoid


-- ********************************  Monoid **********************************************************
-- A monoid is a binary associative operation with an identity.

-- 1. Monoid: The thing we’re talking about—monoids. That’ll end up being the name of our type class.
-- 2. Binary, i.e., two. So, there will be two of something.
-- 3. Associative—this is a property or law that must be satisfied. You’ve seen associativity with addition and multiplication.
--    We’ll explain it more in a moment.

-- 4. Operation—so called because in mathematics, it’s usually used as an infix operator. You can read this interchangeably as “function.” 
-- Note that given the mention of “binary” earlier, we know that this is a function of two arguments.

-- 5. Identity is one of those words in mathematics that pops up a lot. In this context, we can take this to mean there’ll be some value which, 
-- when combined with any other value, will always return that other value. This can be seen most immediately with examples.

-- For lists, we have a binary operator, ++, that joins two lists together. We can also use a function, mappend, 
--  from the Monoid type class to do the same thing

-- ********************************  mappend (from the Monoid type class)  *******************************************************
-- *Purplebook> mappend [1..5] []
-- [1,2,3,4,5]
-- *Purplebook> mappend [] [1..5]
-- [1,2,3,4,5]

-- Prelude> :t mappend
-- mappend :: Monoid a => a -> a -> a


-- Prelude> mappend [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]
-- *Purplebook> mconcat [[1..3], [4..6]]
-- [1,2,3,4,5,6]
-- *Purplebook> let g = " goes well with garlic"
-- *Purplebook> mappend "Trout" g
-- "Trout goes well with garlic"

m2 = mappend [4, 8, 10] mempty
m3 = mappend mempty [5, 9, 11]


-- For lists, the empty list, [], is the identity value:
-- mappend [1..5] [] = [1..5]
-- mappend [] [1..5] = [1..5]

-- Prelude> :t mempty
-- mempty :: Monoid a => a


-- 15.4 How Monoid is defined in Haskell
-- The type class Monoid is defined like this:
-- class Semigroup m => Monoid m where
-- mempty :: m
-- mappend :: m -> m -> m
-- mconcat :: [m] -> m
-- mconcat = foldr mappend mempty



-- 15.5 Examples of using Monoid
-- List
m1 = mappend [1, 2, 3] [4, 5, 6]
-- *Purplebook> m1
-- [1,2,3,4,5,6]
mc1 = mconcat [[1..3], [4..6]]
-- *Purplebook> mc1
-- [1,2,3,4,5,6]
plusplus = (++) [3,2,1] [6,5,4]
f = foldr (++) [] [[10..13], [14..16]]
-- https://byorgey.wordpress.com/2012/11/05/foldr-is-made-of-monoids/

f2 =  foldr mappend mempty [[1..3], [4..6]]

-- 15.6 Why Integer doesn’t have a Monoid
-- To resolve the conflict, we have the Sum and Product newtypes to wrap numeric values and signal which Monoid instance we want.

sum1 = mappend (Sum 1) (Sum 5)
prod1 = mappend (Product 5) (Product 5)

-- Why newtype?

-- Prelude> import Data.Monoid
-- Prelude> :info Sum
-- newtype Sum a = Sum {getSum :: a}
-- ...some instances elided...
-- instance Num a => Monoid (Sum a)

-- Prelude> :info Product
-- newtype Product a =
-- Product {getProduct :: a}
-- ...some instances elided...
-- instance Num a => Monoid (Product a)

x = Sum 1
y = Sum 2
z = Sum 3
sum3 = mappend x (mappend y z)


--  (<>) 
-- Prelude Data.Monoid> :t (<>)
-- (<>) :: Monoid m => m -> m -> m
sum4 = Sum 1 <> Sum 5 <> Sum 10
-- *Purplebook> sum4
-- Sum {getSum = 16}
mcon1  =  mconcat [Sum 8, Sum 10, Sum 21]
-- *Purplebook> mcon1
-- Sum {getSum = 39}

-- *Purplebook> foldr mappend mempty [Sum 8, Sum 12, Sum 9]
-- Sum {getSum = 29}
-- *Purplebook> mappend (Sum 9) mempty
-- Sum {getSum = 9}

--Prelude Data.Monoid> :t mconcat
-- mconcat :: Monoid a => [a] -> a



-- *Purplebook> Sum 1 <> Sum 2 <> Sum 3
-- Sum {getSum = 6}

getsum1 = getSum $ mappend x y
-- *Purplebook> :t getSum
-- getSum :: Sum a -> a
-- *Purplebook> getsum1
-- 3
yprod = Product  5
getprod1 = getProduct $ mappend yprod yprod
-- *Purplebook> getprod1
-- 25

getsum3 = getSum $ mconcat [x, y, z]       -- with $ is just brackets
-- *Purplebook> getsum3
-- 6
getsum4 = getSum (mconcat [x, y, z])
-- *Purplebook> getsum4
-- 6


-- 15.7 Why bother?

-- Monoids are even strongly associated with the concept of folding or catamorphism—something we do all the time in Haskell. 
-- You’ll see this more explicitly in Chapter 20, but here’s a taste:Monoids are even strongly associated with the concept
-- of folding or catamorphism—something we do all the time in Haskell. You’ll see this more explicitly in Chapter 20, but here’s a taste:

xs = [2, 4, 6] :: [Product Int]
fol1 = foldr mappend mempty xs
-- *Purplebook> fol1
-- Product {getProduct = 48}

ys = [2, 4, 6] :: [Sum Int]
sumfol1 = foldr mappend mempty ys
-- *Purplebook> sumfol1
-- Sum {getSum = 12}
-- *Purplebook> getSum sumfol1
-- 12


strList = ["blah", "woot"]
strListFold = foldr mappend mempty strList
-- *Purplebook> strListFold
-- "blahwoot"

-- 15.8 Laws


-- We care about the laws a Monoid instance must adhere to, because we want our programs to be correct wherever possible.
-- Proofs are programs, and programs are proofs. We


-- Monoid instances must abide by the following laws:
-- left identity
-- mappend mempty x = x
-- right identity
-- mappend x mempty = x

-- associativity
-- mappend x (mappend y z) =
-- mappend (mappend x y) z
-- mconcat = foldr mappend mempty

-- Any laws that apply to mappend will also apply to the <> method defined by Semigroup.


-- *****************************    For lists, the empty list, [], is the identity value: *********************************


-- Here is how the identity law looks in practice:
-- -- left identity
leftI = mappend mempty (Sum 1) -- Sum {getSum = 1}

-- -- right identity
rightI = mappend (Sum 1) mempty -- Sum {getSum = 1}


-- We can demonstrate associativity using the infix operator <> from the Semigroup class. Remember that mappend and <> should be identical in behavior
-- Associativity:
associativity1 = (Sum 1) <> (Sum 2 <> Sum 3)
associativity2 = (Sum 1 <> Sum 2) <> (Sum 3)


-- https://www.splashlearn.com/math-vocabulary/addition/associative-property
-- ********************************  Associatiivity  **********************************************************
-- This property states that when three or more numbers are added (or multiplied), 
-- the sum (or the product) is the same regardless of the grouping of the addends (or the multiplicands).
-- Grouping means the use of parentheses or brackets to group numbers.
-- Associative property involves 3 or more numbers.
-- The numbers that are grouped within a parenthesis or bracket become one unit.
-- Associative property can only be used with addition and multiplication and not with subtraction or division.
-- Example of Associative Property for Addition 
-- 4 + (5+3)  = (4 + 5) + 3 = 12

-- some more examples here for identiy and associativity with Lists



-- 15.9 Different instance, same representation

-- Monoid is somewhat different from other type classes in Haskell, in that many datatypes have more than one valid monoid. 
-- We saw that for numbers, both addition and multiplication are sensible monoids with different behaviors. 
-- When we have more than one potential implementation for Monoid for a datatype, it’s most convenient to use newtypes to distinguish them, 
-- as we did with Sum and Product.

-- We mentioned above that monoids are important to folding and catamorphisms, more generally. 

-- the Monoid instances for Bool
-- Boolean values have two possible monoids—a monoid of conjunction and a monoid of disjunction.

bool1 = All True <> All True
bool2 = All True <> All False
bool3 = Any True <> Any False
bool4 = Any False <> Any False

-- Prelude Data.Monoid> :i <>
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--  (<>) :: a -> a -> a
--  ...
--  	-- Defined in ‘GHC.Base’
--infixr 6 <>


-- All represents Boolean conjunction: it returns a True if and only if all values it is “appending” are True. 
-- Any is the monoid of Boolean disjunction: it returns a True if any value is True.

-- Prelude Data.Monoid> :t All
-- All :: Bool -> All
-- Prelude Data.Monoid> :t Any
-- Any :: Bool -> Any

-- The Maybe type has more than two possible Monoids. We’ll look at each in turn, but the two that have an obvious relationship
-- are First and Last.

-- First returns the first or leftmost non-Nothing value:
fir1 = First (Just 1)
fir2 = First (Just 2)
first1 = fir1 `mappend` fir2
fir3 = First (Nothing)
fir4 = First (Just 2)
first2 = fir3 `mappend` fir4
fir5 = First (Nothing)
fir6 = First (Nothing)
first3 = fir5 `mappend` fir6
-- printFirst3 :: First a -> IO ()
-- printFirst3 b = if b == First (Nothing)
--                 then return()
--                 else putStrLn (" Success First ")


foo :: Int -> IO ()
foo a
   | a > 100 = return ()    -- Does not print anything
   | otherwise = putStrLn "YES!!!"



-- 15.10 Reusing algebras by asking for algebras
-- We alluded to there being more possible Monoids for Maybe than just First and Last. Let’s write that other Monoid instance.
-- will now be concerned not with choosing one value out of a set of values but of combining the a values contained within the Maybe a type.

-- notice a pattern:
-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b)
-- => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c)
-- => Monoid (a, b, c)

data Booly a =
    False'
    | True'
    deriving (Eq, Show)

-- conjunction
instance Semigroup (Booly a) where
    (<>) False' _ = False'
    (<>) _ False' = False'
    (<>) True' True' = True'

instance Monoid (Booly a) where
    mempty = True'

booly1 = True'

-- *Purplebook> foldr mappend mempty [booly1, booly1, booly1]
-- True'
-- *Purplebook> foldr mappend mempty [booly1, booly1, False']
-- False'
-- *Purplebook> foldr mappend mempty [True', True',True']
-- True'
-- *Purplebook> :t foldr mappend mempty [True', True',True']
-- foldr mappend mempty [True', True',True'] :: Booly a

-- *Purplebook> mconcat [booly1, booly1, False']
-- False'
-- *Purplebook> mconcat [booly1, booly1, booly1]
-- True'


-- Exercise: Optional Monoid

data Optional a =
       Nada
     | Only a
     deriving (Eq, Show)

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = do
    a <- arbitrary
    elements [Nada, Only a]

-- genOptional :: Gen (Optional String)
-- genOptional = do
--     a <- arbitrary
--     elements [Nada, Only a]

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = genOptional
-- *Purplebook> type G = Gen (Optional Int)
-- *Purplebook> sample' (genOptional :: G)
-- [Only 0,Nada,Only 2,Only 0,Only (-4),Only (-8),Only (-8),Only 9,Nada,Only 17,Nada]

instance Monoid a
     => Monoid (Optional a) where
  mempty = Nada -- undefined
  -- mappend = undefined

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> (Only a') = Only (a <> a')
  -- mempty <> (Only a) = Only a                             
  (Only a) <> Nada = Only a
  Nada <> Nada = Nada

only15 = (Only (Sum 7)) <> (Only (Sum 8))
-- *Purplebook> only15
-- Only (Sum {getSum = 15})

-- intGenOptional :: Gen (Optional (Sum Int))
-- intGenOptional = genOptional

-- *Purplebook> sample' intGenOptional
-- [Only (Sum {getSum = 0}),Only (Sum {getSum = -1}),Nada,Nada,Nada,Nada,Nada,Nada,Only (Sum {getSum = 14}),Only (Sum {getSum = 1}),Only (Sum {getSum = -3})]


propOptionalCommutative :: (Eq a, Semigroup a) => a -> a -> Bool
propOptionalCommutative a b = a <> b  == b <> a
pp :: IO ()
pp = quickCheck ( propOptionalCommutative :: (Optional (Sum Int) -> Optional (Sum Int) -> Bool))
-- *Purplebook> verboseCheck ( propOptionalCommutative :: (Optional (Sum Int) -> Optional (Sum Int) -> Bool))
-- Passed:  
-- Only (Sum {getSum = 0})
-- Only (Sum {getSum = 0})

-- Passed: 
-- Only (Sum {getSum = 0})
-- Nada
-- .....
-- ....
-- +++ OK, passed 100 tests.

chM :: IO ()
chM = quickCheck ( propOptionalCommutative :: (Optional String -> Optional String -> Bool))
-- this will fail since mappend with String is NOT commutative
-- *Purplebook> chM
-- *** Failed! Falsified (after 12 tests):  
-- Only "\7914\a\988970\DEL"
-- Only "Gy3\62851!"

-- Gave error w/o the Semi group.Below link gave solution - thats cause Monoid means Semegroup has to exist.
-- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

onlySum = Only (Sum 1)
opt1 = onlySum `mappend` onlySum
-- *Purplebook> opt1
-- Only (Sum {getSum = 2})
onlyFourP = Only (Product 4)
onlyTwoP = Only (Product 2)
prodOpt1 = onlyFourP `mappend` onlyTwoP



propMonoidAssocOptionalProd a b =
    (Only (Product a)) `mappend` (Only (Product b)) == (Only (Product b)) `mappend` (Only (Product a))
-- *Purplebook> quickCheck propMonoidAssocOptionalProd
-- +++ OK, passed 100 tests.

sumNada = Only (Sum 1) `mappend` Nada
-- *Purplebook> sumNada 
-- Only (Sum {getSum = 1})
sum1Nada = Only [1] `mappend` Nada
-- *Purplebook> sum1Nada
-- Only [1]
sum1_2 = Only [1] `mappend` Only [2]    -- Mine - these are list examples 
-- *Purplebook> sum1_2
-- Only [1,2]


-- this below is my own practive where nada with anything is nada. so slight variation from what was done earlier.
data Optional2 a =
       Nada2
     | Only2 a
     deriving (Eq, Show)
instance Monoid a
     => Monoid (Optional2 a) where
  mempty = Nada2 -- undefined
  -- mappend = undefined


instance Semigroup a => Semigroup (Optional2 a) where
  Nada2 <> (Only2 a) = Nada2
  (Only2 a) <> Nada2 = Nada2
  (Only2 a) <> (Only2 a') = Only2 (a <> a')
  Nada2 <> Nada2 = Nada2

only2Sum = Only2 (Sum 1)
opt21 = only2Sum `mappend` only2Sum
-- *Purplebook> opt21
-- Only2 (Sum {getSum = 2})


sum2Nada = Only2 (Sum 1) `mappend` Nada2
-- *Purplebook> sum2Nada 
-- Nada2

sum21Nada = Only2 [1] `mappend` Nada2
-- *Purplebook> sum21Nada
-- Nada2

sum21_2 = Only2 [1] `mappend` Only2 [2]
-- *Purplebook> sum21_2
-- Only2 [1,2]


data BlockChainX a b c =
         BlockChainX { blockNo' :: a,
                        blockHeader' :: b,
                        data1' :: c}
                    deriving (Eq, Show)

data BlockChain a =
         BlockChain { blockNo :: a,
                        blockHeader :: a,
                        data1 :: a}
                    deriving (Eq, Show)

instance (Num a, Ord a, Enum a) => Semigroup (BlockChain a) where
  BlockChain { blockNo = x1, blockHeader = x2, data1 = x3 } <> BlockChain { blockNo = y1, blockHeader = y2, data1 = y3 } = BlockChain { blockNo = (if x1 > y1 then x1 else y1), blockHeader = (if x2 > y2 then x2 else y2), data1 = (x3+y3) }

-- The Monoid instance for numbers under addition
instance (Num a, Ord a, Enum a) => Monoid (BlockChain a)
  where
    mempty = BlockChain {blockNo =0, blockHeader =0, data1 =0}
    -- x `mappend` y = BlockChain { blockNo = (x+y), blockHeader = (x+y), data1 = (x+y)}

instance (Num a, Ord a, Enum a) => Arbitrary (BlockChain a) where
    -- arbitrary = elements [BlockChain { blockNo = 0, blockHeader = 0, data1 = 0 } , BlockChain { blockNo = 100, blockHeader = 100, data1 = 100 }]
    arbitrary = elements (L.map arbBlocks [1..5])

arbBlocks :: (Num a) => a -> BlockChain a
arbBlocks i = BlockChain { blockNo = i, blockHeader = i, data1 = i}

blockChain1 = BlockChain { blockNo = 3, blockHeader = 2, data1 = 100}
blockChain2 = BlockChain { blockNo = 5, blockHeader = 8, data1 = 300}

-- with record type declaration you can also directly use BlockChain 4 9 10 instead of BlockChain { blockNo = 4, blockHeader = 9, data1 = 10}
-- *Purplebook> bl = BlockChain 4 9 10 
-- *Purplebook> bl
-- BlockChain {blockNo = 4, blockHeader = 9, data1 = 10}


-- instance Semigroup a => Semigroup (BlockChain a) where
--   BlockChain { blockNo a, blockHeader a, data1 a } <> BlockChain { blockNo 0, blockHeader 0, data1 0 } = BlockChain { blockNo a, blockHeader a, data1 a }
--   BlockChain { blockNo 0, blockHeader 0, data1 0 } <> BlockChain { blockNo a, blockHeader a, data1 a } = BlockChain { blockNo a, blockHeader a, data1 a }
--   BlockChain { blockNo a, blockHeader a, data1 a } <> BlockChain { blockNo a', blockHeader a', data1 a' } = BlockChain { blockNo a a', blockHeader a, data1 a }
--   BlockChain { blockNo 0, blockHeader 0, data1 0 } <> BlockChain { blockNo 0, blockHeader 0, data1 0 } = BlockChain { blockNo 0, blockHeader 0, data1 0 }

-- instance Num a => Monoid (BlockChain a) where
--          mempty = BlockChain { blockNo = 0, blockHeader = 0, data1 = 0}
--   -- mappend = undefined


-- instance Semigroup a => Semigroup (BlockChain a) where
--   NoBlock <>  ((BlockNo a) (BlockHeader a) (Data a)) = ((BlockNo a) (BlockHeader a) (Data a))
--   ((BlockNo a) (BlockHeader a) (Data a)) <> NoBlock = ((BlockNo a) (BlockHeader a) (Data a))
--   ((BlockNo a) (BlockHeader a) (Data a)) <> ((BlockNo a') (BlockHeader a') (Data a')) = (BlockNo a <> a') (BlockHeader a <> a') (Data a <> a'))
--   NoBlock <> NoBlock = NoBlock









-- 15.11 Madness

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String

madlibbinBetter' e adv noun adj =
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

madlib1 = madlibbinBetter' "Blacky" "while" "Dodge" "pretty"
-- *Purplebook> madlib1
-- "Blacky! he said while as he jumped into his car Dodge and drove off with his pretty wife."

-- 15.12 Better living through QuickCheck

-- Proving laws can be tedious, especially if the code we’re checking is in the middle of changing frequently.
-- Accordingly, having a cheap way to get a sense of whether or not the laws are likely to be obeyed by an instance is pretty useful. 
-- QuickCheck happens to be an excellent way to accomplish this.

-- Validating associativity with QuickCheck

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
            (a <> (b <> c)) == ((a <> b) <> c)
type S = String
type Bol = Bool
type MA = S -> S -> S -> Bol

quickcheck1 :: IO ()
quickcheck1 = quickCheck (monoidAssoc :: MA)   -- +++ OK, passed 100 tests.
-- *Purplebook> quickcheck1 
-- +++ OK, passed 100 tests.

type S' = Sum Int
type MA' = S' -> S' -> S' -> Bol
quickcheck2 = quickCheck (monoidAssoc :: MA')
-- *Purplebook> quickcheck2
-- +++ OK, passed 100 tests.

type Bl = BlockChain Integer
type MABl = Bl -> Bl -> Bl -> Bol
quickcheck3 = quickCheck (monoidAssoc :: MABl)
-- *Purplebook> quickcheck3
-- +++ OK, passed 100 tests.

-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Eq.html
-- The Eq class defines equality (==) and inequality (/=). All the basic datatypes exported by the Prelude are instances of Eq, 
--     and Eq may be derived for any datatype whose constituents are also instances of Eq.

-- *Purplebook> verboseCheck (monoidAssoc :: MA')
-- Passed:  
-- Sum {getSum = 0}
-- Sum {getSum = 0}
-- Sum {getSum = 0}

-- Passed: 
-- Sum {getSum = 0}
-- Sum {getSum = 1}
-- Sum {getSum = -1}

-- Passed:  
-- Sum {getSum = 2}
-- Sum {getSum = 1}
-- Sum {getSum = 2}
-- ....... .....
-- ....... .....
-- +++ OK, passed 100 tests.






-- Testing left and right identity


monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a
mli = monoidLeftIdentity

mri = monoidRightIdentity

-- *Purplebook> quickCheck (mli :: String -> Bool)
-- +++ OK, passed 100 tests.
-- *Purplebook> quickCheck (mri :: String -> Bool)
-- +++ OK, passed 100 tests.


-- Testing QuickCheck’s patience
-- Let us see an example of QuickCheck catching us out for having an invalid Monoid.
-- We’re going to demonstrate why a Bool Monoid can’t have False as the identity, always returning the value False, and still be a valid Monoid.

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

-- *Purplebook> Fools <> Twoo
-- Fools
-- *Purplebook> Twoo <> Twoo
-- Fools

instance Monoid Bull where
    mempty = Fools
-- *Purplebook> Fools <> mempty
-- Fools
-- *Purplebook> Twoo <> mempty    --- that's why its not Identity
-- Fools

type BullMappend =
    Bull -> Bull -> Bull -> Bool

-- *Purplebook> quickCheck (monoidAssoc :: BullMappend)
-- +++ OK, passed 100 tests.
-- *Purplebook> quickCheck (monoidLeftIdentity :: Bull -> Bool)
-- *** Failed! Falsified (after 2 tests):  
-- Twoo
-- *Purplebook> quickCheck (monoidRightIdentity:: Bull -> Bool)
-- *** Failed! Falsified (after 1 test):  
-- Twoo


-- My own test from • https://youtu.be/bsp5pJlw6R0
-- 1440- create your own  getSum from Sum like.. will be good exercise  ---

-- newtype SumCh a = SumCh { getSumCh :: a}
--         deriving (Eq , Ord)
-- instance Num a => Semigroup (SumCh a) where
--     (<>) = coerce ((+) :: a -> a-> a)
--     -- stimes n (SumCh a) = SumCh (fromIntegral n * a)
-- instance Num a => Monoid (SumCh a) where
--     mempty = SumCh 0



-- Exercise: Maybe another Monoid


newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
-- (<>) = undefined
  First' Nada <> First' (Only a) = First' (Only a )
  First' (Only a) <> First' (Only a') = First' (Only a)
  -- mempty <> (Only a) = Only a                             
  First' (Only a) <> First' Nada = First' (Only a )
  First' Nada <> First' Nada = First' Nada


instance Monoid (First' a) where
    mempty = First' Nada

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
    a <- arbitrary
    return (First' a)
-- *Purplebook> sample' (genFirst' :: Gen (First' (Sum Int)))
-- [First' {getFirst' = Nada},First' {getFirst' = Nada},First' {getFirst' = Only (Sum {getSum = -2})},
--      First' {getFirst' = Nada},First' {getFirst' = Only (Sum {getSum = -2})},First' {getFirst' = Nada},
--      First' {getFirst' = Nada},First' {getFirst' = Nada},First' {getFirst' = Only (Sum {getSum = -7})},
------- First' {getFirst' = Only (Sum {getSum = -1})},First' {getFirst' = Nada}]


instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = genFirst'





-- instance Arbitrary a =>
--         Arbitrary (First' a) where
--     arbitrary =
--         frequency [(1, return First' Nada),
--                   (3, liftM First'  arbitrary)]


-- instance Arbitrary (First' a) where
-- -- --     -- arbitrary = elements [BlockChain { blockNo = 0, blockHeader = 0, data1 = 0 } , BlockChain { blockNo = 100, blockHeader = 100, data1 = 100 }]
-- --      -- arbitrary = elements (L.map arbFirst' [1..5])
-- --      -- arbitrary = elements [First' (Only "sample")]
--     arbitrary =
--       frequency [ (100, return First' Nada) ]
--                     (1, return First' Nada) ]
-- instance (Num a, Ord a, Enum a) => Arbitrary (BlockChain a) where
--     -- arbitrary = elements [BlockChain { blockNo = 0, blockHeader = 0, data1 = 0 } , BlockChain { blockNo = 100, blockHeader = 100, data1 = 100 }]
--     arbitrary = elements (L.map arbBlocks [1..5])

-- arbBlocks :: (Num a) => a -> BlockChain a
-- arbBlocks i = BlockChain { blockNo = i, blockHeader = i, data1 = i}

-- instance Arbitrary Bull where
-- arbitrary =
-- frequency [ (1, return Fools)
-- , (1, return Twoo) ]


arbFirst' :: Char -> First' Char
arbFirst' i = First' (Only i)


firstMappend :: First' a
    -> First' a
    -> First' a
firstMappend = mappend

a = First' (Only "rafa")
b = First' (Only "Djokovic")
-- *Purplebook> mappend a b
-- First' {getFirst' = Only "rafa"}
-- *Purplebook> mappend b a
-- First' {getFirst' = Only "Djokovic"}

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
   First' String -> Bool

-- " Monoid Assoc FirstMappend test: "
-- +++ OK, passed 100 tests.
-- " Monoid Left Identity test: FstId "
-- +++ OK, passed 100 tests.
-- " Monoid Right Identity test: FstId "
-- +++ OK, passed 100 tests.


onlyOne = First' (Only 1)
onlyTwo = First' (Only 2)
nada = First' Nada

-- *Purplebook> onlyOne `mappend` nada
-- First' {getFirst' = Only 1}
-- *Purplebook> nada `mappend` nada
-- First' {getFirst' = Nada}
-- *Purplebook> nada `mappend` onlyTwo
-- First' {getFirst' = Only 2}
-- *Purplebook> onlyOne `mappend` onlyTwo
-- First' {getFirst' = Only 1}


-- 15.13 Semigroup

-- **************************************************** Semigroup ******************************************************

-- a monoid= to a semigroup, we simply no longer furnish nor require an identity. The core operation remains binary and associative.
-- In that sense, it’s a weaker algebra.
-- our definition of Semigroup is:
-- class Semigroup a where
--     (<>) :: a -> a -> a

-- And we’re left with one law:
-- (a <> b) <> c = a <> (b <> c)

-- **************************************************** NonEmpty datatype ******************************************************

-- NonEmpty, a useful datatype
-- One useful datatype that can’t have a Monoid instance but does have a Semigroup instance is the NonEmpty list type.
-- It is a list datatype that can never be an empty list:


-- from Data.List.NonEmpty
-- data NonEmpty a = a :| [a]

-- **************************************************** :| infix data constructor  ******************************************************
-- Here :| is an infix data constructor that takes two (type) arguments. It’s a product of a and [a]. 
-- It guarantees that we always have at least one value of type a, which [a] does not guarantee, as any list might be empty.

-- Because  NonEmpty is a product of two arguments, we could’ve also written it as:
newtype NonEmpty1 a =
    NonEmpty1 (a, [a])
    deriving (Eq, Ord, Show)


-- Prefix, works.
data P =
    Prefix Int String deriving Show
-- *Purplebook> x = Prefix 5 "Rafa"
-- *Purplebook> x
-- Prefix 5 "Rafa"

-- Infix, works.
data Q =
    Int :!!: String deriving Show
-- *Purplebook> q = 5 :!!: "rafa"
-- *Purplebook> q
-- 5 :!!: "rafa"


prodNE1 = 1 :| [2,3,4]
-- *Purplebook> prodNE1
-- 1 :| [2,3,4]
prodNE2 = 5 :| [6,7,8]
prodNE3 = prodNE1 <> prodNE2
-- *Purplebook> prodNE3
-- 1 :| [2,3,4,5,6,7,8]
-- prodElm1 = head 
headProdNE3 = N.head prodNE3
-- *Purplebook> headProdNE3
-- 1
tailProdNE3 = N.tail prodNE3
-- *Purplebook> tailProdNE3
-- [2,3,4,5,6,7,8]
lenProdNE3 = N.length prodNE3
-- *Purplebook> lenProdNE3
-- 8

-- Prelude Data.List.NonEmpty Data.Semigroup> :t 1 :| [2, 3]
-- 1 :| [2, 3] :: Num a => NonEmpty a



-- https://wiki.haskell.org/Data.Semigroup

-- 15.15 Chapter exercises

-- Semigroup exercises

-- Given a datatype, implement the Semigroup instance. Add Semigroup constraints to type variables where needed. Use the
-- Semigroup class from base or write your own. When we use <>, we mean the infix mappend operation from the Semigroup type
-- class.
-- Note - We’re not always going to derive every instance you may want or need in the datatypes we provide for exercises.
-- We expect you to know what you need and to take care of it yourself by this point.

-- Ex 1 - 
-- Validate all of your instances with QuickCheck. Since the only law is associativity, that’s the only property you need
-- to reuse. Keep in mind that you’ll potentially need to import the modules for Monoid and Semigroup and to avoid
-- naming conflicts for <>, depending on your version of GHC:

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)


instance Arbitrary Trivial where
    arbitrary = return Trivial


semigroupAssoc :: (Eq m, Semigroup m)
    => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

sgAssoc :: IO ()
sgAssoc = quickCheck ( semigroupAssoc :: (Trivial -> Trivial -> Trivial -> Bool))
sgTrvAssoc = quickCheck ( semigroupAssoc :: TrivAssoc)
-- *Purplebook> sgAssoc 
-- +++ OK, passed 100 tests.
-- *Purplebook> sgTrvAssoc 
-- +++ OK, passed 100 tests.
-- reference chM = quickCheck ( propOptionalCommutative :: (Optional String -> Optional String -> Bool))
-- https://stackoverflow.com/questions/39232294/how-to-write-a-semigroup-instance


-- Already declared before
trvMli = monoidLeftIdentity
trvMlr = monoidRightIdentity

qcMli = quickCheck (trvMli :: Trivial -> Bool)
qcMlr = quickCheck (trvMlr :: Trivial -> Bool)
-- *MonoidsSemigroups> qcMli
-- +++ OK, passed 100 tests.
-- *MonoidsSemigroups> qcMlr
-- +++ OK, passed 100 tests.



-- Ex 2. 
newtype Identity a = Identity a deriving (Eq , Show)
id1 = id (Sum 1)
id2 = id (Sum 2)

xid5 = Identity 5

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
    a <- arbitrary
    return (Identity a)

-- this is if you only need a sample for Identiry itself - not as monoid
identityGenInt ::   Gen (Identity Int)
identityGenInt = genIdentity
-- *MonoidsSemigroups> sample' identityGenInt
-- [Identity 0,Identity 1,Identity (-4),Identity (-2),Identity 2,Identity 5,Identity (-10),Identity 11,
--       Identity (-3),Identity (-8),Identity 5]

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity a' = Identity (a <> a')

-- instance Semigroup a => Monoid (Identity a) where
--     mempty = (Identity a')
--     mappend mempty (Identity a) = Identity a
--     mappend (Identity a) mempty = Identity a

-- -- because 'a' is monoid it has mempty - also we give Monoid Constraint as only that has mempty and Semigroup constrain would not work.
instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty         
    mappend = (<>)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genIdentity

type IdAssocSumInt =
    Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool



type IdAssocString =
    Identity String -> Identity String -> Identity String -> Bool

sgIdAssocSumInt :: IO ()
sgIdAssocSumInt = quickCheck ( semigroupAssoc :: IdAssocSumInt)
-- *Purplebook> sgIdAssocSumInt 
-- +++ OK, passed 100 tests.

sgIdAssocString = quickCheck ( semigroupAssoc :: IdAssocString)
-- *Purplebook> sgIdAssocString
-- +++ OK, passed 100 tests.


identityMli = monoidLeftIdentity
identitytrvMlr = monoidRightIdentity

qcIdMli = quickCheck (identityMli :: Identity (Sum Int) -> Bool)
qcIdMlr = quickCheck (identitytrvMlr :: Identity (Sum Int) -> Bool)


-- *MonoidsSemigroups> qcIdMli
-- +++ OK, passed 100 tests.
-- *MonoidsSemigroups> qcIdMlr
-- +++ OK, passed 100 tests.

-- Ex 3. data Two a b = Two a b
-- Hint: Ask for another Semigroup instance.
data Two a b = Two a b deriving (Eq, Show)

genTwo :: (Arbitrary a, Arbitrary b)  => Gen (Two a b)
genTwo = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')



instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

type TwoAssocSumInt =
    Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Bool


sgTwoAssocSumInt = quickCheck ( semigroupAssoc :: TwoAssocSumInt)
-- *Purplebook> sgTwoAssocSumInt
-- +++ OK, passed 100 tests.

-- verboseCheck
-- *Purplebook> verboseCheck ( semigroupAssoc :: TwoAssocSumInt)
-- Passed:   
-- Two (Sum {getSum = 84}) (Sum {getSum = 7})
-- Two (Sum {getSum = 19}) (Sum {getSum = -38})
-- Two (Sum {getSum = -67}) (Sum {getSum = -92})
-- +++ OK, passed 100 tests.

type TwoAssocProdInt =
    Two (Product Int) (Product Int) -> Two (Product Int) (Product Int) -> Two (Product Int) (Product Int) -> Bool

sgTwoAssocProductInt = quickCheck ( semigroupAssoc :: TwoAssocProdInt)
-- *Purplebook> sgTwoAssocProductInt
-- +++ OK, passed 100 tests.
-- *Purplebook> Two (Product {getProduct = -57}) (Product {getProduct = 44}) <> Two (Product {getProduct = 59}) (Product {getProduct = -86})
-- Two (Product {getProduct = -3363}) (Product {getProduct = -3784})
-- *Purplebook> Two (Product {getProduct = 59}) (Product {getProduct = -86}) <> Two (Product {getProduct = -57}) (Product {getProduct = 44})
-- Two (Product {getProduct = -3363}) (Product {getProduct = -3784})



twoMli = monoidLeftIdentity
twoMlr = monoidRightIdentity

qcTwoMli = quickCheck (twoMli :: Two (Sum Int) (Sum Int) -> Bool)
qcTwoMlr = quickCheck (twoMlr :: Two (Sum Int) (Sum Int) -> Bool)
-- *MonoidsSemigroups> qcTwoMli
-- +++ OK, passed 100 tests.
-- *MonoidsSemigroups> qcTwoMlr
-- +++ OK, passed 100 tests.

twoMliStr = monoidLeftIdentity
qcTwoMliStr = quickCheck (twoMliStr :: Two (String) (String) -> Bool)
-- *MonoidsSemigroups> qcTwoMliStr
-- +++ OK, passed 100 tests.

twoStr1 = Two "Rafa" "Nadal"
twoStr2 = Two " TheGoat" " TheGreatest"
twoStr4 = ( (Two mempty mempty ) :: Two String String)
twoStr3 = twoStr1 <> twoStr2
-- *MonoidsSemigroups> twoStr3
-- Two "Rafa TheGoat" "Nadal TheGreatest"

-- *MonoidsSemigroups> twoStr1 <> twoStr4
-- Two "Rafa" "Nadal"

twoStr5 = (Two mempty "djoker" ) :: Two String String
-- *MonoidsSemigroups> twoStr5 <> twoStr1
-- Two "Rafa" "djokerNadal"


-- Ex 4
-- 4. data Three a b c = Three a b c
data Three a b c = Three a b c deriving (Eq, Show)


genThree :: (Arbitrary a,  Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary      -- <$> came from hint of Visual code editor

-- semigroup we define just the mappend 
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

-- declare a concretized type for quickCheck test
type ThreeAssocSumInt =
    Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Bool

sgThreeAssocSumInt = quickCheck ( semigroupAssoc :: ThreeAssocSumInt)
-- *Purplebook> sgThreeAssocSumInt
-- +++ OK, passed 100 tests.
-- *Purplebook> verboseCheck ( semigroupAssoc :: ThreeAssocSumInt)
-- Passed:   
-- Three (Sum {getSum = 62}) (Sum {getSum = -77}) (Sum {getSum = -49})
-- Three (Sum {getSum = 53}) (Sum {getSum = -19}) (Sum {getSum = 96})
-- Three (Sum {getSum = -80}) (Sum {getSum = -34}) (Sum {getSum = -86})

-- +++ OK, passed 100 tests.


type ThreeAssocMaybeSumInt =
    Three (Maybe (Sum Int)) (Maybe (Sum Int)) (Maybe (Sum Int)) -> 
          Three (Maybe (Sum Int)) (Maybe (Sum Int)) (Maybe (Sum Int)) -> 
                Three (Maybe (Sum Int)) (Maybe (Sum Int)) (Maybe (Sum Int)) -> Bool
sgThreeAssocMaybeSumInt = quickCheck ( semigroupAssoc :: ThreeAssocMaybeSumInt)
-- *Purplebook> sgThreeAssocMaybeSumInt
-- +++ OK, passed 100 tests.

-- verboseCheck ( semigroupAssoc :: ThreeAssocMaybeSumInt)
-- Passed:   
-- Three (Just (Sum {getSum = 42})) (Just (Sum {getSum = -94})) (Just (Sum {getSum = 67}))
-- Three (Just (Sum {getSum = 25})) (Just (Sum {getSum = 68})) (Just (Sum {getSum = 69}))
-- Three (Just (Sum {getSum = -96})) Nothing (Just (Sum {getSum = -44}))

-- +++ OK, passed 100 tests.

-- -- Ex 5- 
-- -- 5. data Four a b c d = Four a b c d
-- Similar to exercise 3 and 4. Will do later. 


-- Ex 6. newtype BoolConj =
-- BoolConj Bool
-- What it should do:
-- Prelude> (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- Prelude> (BoolConj True) <> (BoolConj False)
-- BoolConj False

newtype BoolConj =
    BoolConj Bool deriving (Eq, Show)

-- genBoolConj :: Gen (BoolConj Bool)
genBoolConj :: Gen BoolConj
genBoolConj = do
    a <- arbitrary
    return (BoolConj a)

-- define instance of arbitrary
instance Arbitrary BoolConj where
    arbitrary = genBoolConj 

-- semigroup we define just the mappend 
instance Semigroup (BoolConj) where
    BoolConj True <> BoolConj True = BoolConj True
    BoolConj False <> _ = BoolConj False
    _ <> BoolConj False = BoolConj False

-- declare a concretized type for quickCheck test
type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

boolConjAssoc = quickCheck ( semigroupAssoc :: BoolConjAssoc)
-- *Purplebook> boolConjAssoc
-- +++ OK, passed 100 tests.
-- verboseCheck ( semigroupAssoc :: BoolConjAssoc)
-- Passed:   
-- BoolConj False
-- BoolConj True
-- BoolConj False
-- +++ OK, passed 100 tests.

-- *Purplebook> (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- *Purplebook> (BoolConj True) <> (BoolConj False)
-- BoolConj False


-- Ex 7. newtype BoolDisj =
-- BoolDisj Bool
-- What it should do:
-- Prelude> (BoolDisj True) <> (BoolDisj True)
-- BoolDisj True
-- Prelude> (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True


newtype BoolDisj =
    BoolDisj Bool deriving (Eq, Show)


genBoolDisj :: Gen BoolDisj
genBoolDisj = do
    a <- arbitrary
    return (BoolDisj a)

-- define instance of arbitrary
instance Arbitrary BoolDisj where
    arbitrary = genBoolDisj

-- semigroup we define just the mappend 
instance Semigroup (BoolDisj) where
    BoolDisj False <> BoolDisj False = BoolDisj False
    _ <> _ = BoolDisj True


-- declare a concretized type for quickCheck test
type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

boolDisjAssoc = quickCheck ( semigroupAssoc :: BoolDisjAssoc)
-- *Purplebook> boolDisjAssoc
-- +++ OK, passed 100 tests.
-- *Purplebook> verboseCheck ( semigroupAssoc :: BoolDisjAssoc)
-- Passed:   
-- BoolDisj True
-- BoolDisj False
-- BoolDisj True

-- +++ OK, passed 100 tests.

-- *Purplebook> (BoolDisj True) <> (BoolDisj True)
-- BoolDisj True
-- *Purplebook> (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True


-- ioString :: IO () -> String
-- ioString x = do
--         str1 <- x
--         putStrLn (" printing Str1: " ++ str1)
s = verboseCheck ( semigroupAssoc :: ThreeAssocMaybeSumInt)

-- convertIOString :: IO () -> String
-- convertIOString a = 


-- Ex 8. data Or a b =
-- Fst a
-- | Snd b
-- The Semigroup for Or should have the following behavior. We can think of it as having a “sticky” Snd value, whereby
-- it’ll hold onto the first Snd value when and if one is passed as an argument. This is similar to the First' Monoid you
-- wrote earlier:
-- Prelude> Fst 1 <> Snd 2
-- Snd 2
-- Prelude> Fst 1 <> Fst 2
-- Fst 2
-- Prelude> Snd 1 <> Fst 2
-- Snd 1
-- Prelude> Snd 1 <> Snd 2
-- Snd 1

data Or a b =
    Fst a
    | Snd b deriving (Eq, Show)

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do 
    a <- arbitrary 
    b <- arbitrary
    oneof [return $ Fst a,
           return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = genOr 

genOrInt :: Gen (Or Int Int)
genOrInt = genOr
-- *MonoidsSemigroups> sample' genOrInt
-- [Fst 0,Fst 0,Snd (-1),Snd 1,Fst (-1),Fst 4,Fst 12,Snd 2,Snd (-13),Fst (-16),Fst 0]

-- instance (Semigroup a, Semigroup b) => Semigroup (Or a b ) where   
    -- because Or is semigroup but not a b. since we are not doing a <> a' unlike in some other ones above like Two a b
    --    so we dont need semigroup constraing on a and b. We just directly say Or is semigroup but not a and b.
    --       becuase the true interpretation if you put Semigroup contraint menas Or and a and b are all semigroups 
        --      but thats not the case here. 
instance  Semigroup (Or a b ) where
    (Fst a ) <> (Fst a') = Fst a'
    (Fst a ) <> (Snd b') = Snd b' 
    (Snd b ) <> (Fst a') = Snd b
    (Snd b ) <> (Snd b') = Snd b
     
type OrInt =
    (Or Int Int) -> (Or Int Int) -> (Or Int Int) -> Bool

orAssocInt = quickCheck ( semigroupAssoc :: OrInt)
-- *MonoidsSemigroups> orAssocInt
-- +++ OK, passed 100 tests.

-- *MonoidsSemigroups> verboseCheck ( semigroupAssoc :: OrInt)
-- Passed:   
-- Fst 91
-- Snd 2
-- Snd 32
-- +++ OK, passed 100 tests.

-- *MonoidsSemigroups> Fst 1 <> Snd 2
-- Snd 2
-- *MonoidsSemigroups> Fst 1 <> Fst 2
-- Fst 2
-- *MonoidsSemigroups> Snd 1 <> Fst 2
-- Snd 1
-- *MonoidsSemigroups> Snd 1 <> Snd 2
-- Snd 1


-- 9. newtype Combine a b =
-- Combine { unCombine :: (a -> b) }
-- What it should do:
-- Prelude> f = Combine $ \n -> Sum (n + 1)
-- Prelude> g = Combine $ \n -> Sum (n - 1)
-- Prelude> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- Prelude> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- Prelude> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- Prelude> unCombine (g <> f) $ 1
-- Sum {getSum = 2}
-- Hint: This function will eventually be applied to a single value of type a. But you’ll have multiple functions that can
-- produce a value of type b. How do we combine multiple values so we have a single b? This one will probably be
-- tricky! Remember that the type of the value inside of Combine is that of a function. The type of functions should
-- already have an Arbitrary instance that you can reuse for testing this instance.

-- small test to revist record syntax
newtype RecordTest a = 
    RecordTest { fieldlabel1 :: a} deriving (Eq, Show)
record1 = RecordTest 25
-- *MonoidsSemigroups> record1
-- RecordTest {fieldlabel1 = 25}
-- *MonoidsSemigroups> fieldlabel1 record1
-- 25

func1 :: Int -> Int 
func2 :: Int -> Int
func1 n = n + 1
func2 n = n + 2

val0 :: (Int -> Int) -> (Int -> Int) -> Int -> Int
val0 f1 f2 i = f1 (f2 i)

preVal0 = val0 func1 func2
-- *MonoidsSemigroups> preVal0 25
-- 28
val2 = (func1) (func2 5)
-- *MonoidsSemigroups> val2
-- 8

newtype Combine a b =
    Combine { unCombine :: (a -> b) } 

instance Show (Combine a b) where      -- can show a function so you should intialize something random so it does not complain of lack of Show
    show f = "Unicorns!!"

fCmb = Combine $ \n -> Sum (n + 1)
fMemptyCmbSum = Combine $ \(Sum n) -> Sum 0


fSumCmb1 = Combine $ \(Sum n) -> Sum (n + 1)
fSumCmb2 = Combine $ \(Sum n) -> Sum (n - 3)
-- *MonoidsSemigroups> (unCombine fSumCmb1) (Sum 2)
-- Sum {getSum = 3}
gCmb = Combine $ \n -> Sum (n - 1)
-- *MonoidsSemigroups> (unCombine fCmb) 2
-- Sum {getSum = 3}
ufMemptyCmbSum = unCombine fMemptyCmbSum
-- *MonoidsSemigroups> ufMemptyCmbSum (Sum 21)
-- Sum {getSum = 0}

valRtIdComb1 = ( unCombine (fSumCmb1 <> fMemptyCmbSum) ) (Sum 21)     -- mempty keeps only fSumCmb1
-- *MonoidsSemigroups> valRtIdComb
-- Sum {getSum = 22}

valRtIdComb2 = ( unCombine (fSumCmb2 <> fMemptyCmbSum) ) (Sum 21) 
-- *MonoidsSemigroups> valRtIdComb2
-- Sum {getSum = 18}


valLtIdComb2 = ( unCombine (fMemptyCmbSum <> fSumCmb2) ) (Sum 21) 
-- *MonoidsSemigroups> valLtIdComb2
-- Sum {getSum = 18}


ufCmb = (unCombine fCmb)
ugCmb = (unCombine gCmb)
totCmb = ufCmb (ugCmb 7)
-- *MonoidsSemigroups> totCmb
-- Sum {getSum = Sum {getSum = 7}}
valFSumCmb1 = (unCombine fSumCmb1) (Sum 25)
-- *MonoidsSemigroups> valFSumCmb1
-- Sum {getSum = 26}




-- tot2Cmb = ufCmb (getSum (ugCmb 7))
-- *MonoidsSemigroups> totCmb
-- Sum {getSum = Sum {getSum = 7}}


-- https://stackoverflow.com/questions/50629967/is-it-possible-to-generate-arbitrary-functions-in-quickcheck
-- https://www.youtube.com/watch?v=CH8UQJiv9Q4

-- genCombine :: (Arbitrary a, Arbitrary b) => Gen (Combine a b) 
-- genCombine = do
--     a <- arbitrary 
--     b <- arbitrary
--     return (Combine (a -> b))

-- since i dont need unpack the Combine just f was fine.
-- below solution from https://stackoverflow.com/questions/44796668/semigroup-with-data-type-that-contains-function
instance (Semigroup b) 
  => Semigroup (Combine a b) where 
  Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
        mempty = Combine {unCombine=f} where 
                     f a = mempty

instance (CoArbitrary b, CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- Test.QuickCheck.arbitrary
        return (Combine f)

-- instance Eq (Combine a b) where
--     (==) (Combine {unCombine= f}) (Combine {unCombine=g}) = 



combineAssoc :: (Semigroup b, Eq b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc v a b c = (unCombine (a <> (b <> c)) $ v) == (unCombine ((a <> b) <> c) $ v)

type CombineSumIntTest =
     Sum Int -> Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int) -> Bool

combineAssocSumInt = quickCheck ( combineAssoc :: CombineSumIntTest)
-- *MonoidsSemigroups> combineAssocSumInt
-- +++ OK, passed 100 tests.

type CombineSumIntTestIdentity =
     Sum Int -> Combine (Sum Int) (Sum Int) -> Bool

combineRightIdentity :: (Monoid b, Eq b) => a -> Combine a b -> Bool
combineRightIdentity v a = (unCombine (a <> mempty) $ v) == ((unCombine a) $ v)
combineRightIdentitySumInt = quickCheck ( combineRightIdentity :: CombineSumIntTestIdentity)
-- *MonoidsSemigroups> combineRightIdentitySumInt
-- +++ OK, passed 100 tests.

-- combineMonoidLeftIdentity :: (Eq m, Monoid m)
--                    => m
--                    -> Bool
-- combineMonoidLeftIdentity a = ( (mempty ) <> a) == a
-- combineMonoidRightIdentity :: (Eq m, Monoid m)
--                    => m
--                    -> Bool
-- combineMonoidRightIdentity a = (a <> mempty) == a

-- combineMli = combineMonoidLeftIdentity
-- combineMlr = combineMonoidRightIdentity

-- qcCombineMli = quickCheck (combineMli :: Combine (Int ) (Sum Int) -> Bool)
-- qcCombineMlr = quickCheck (combineMli :: Combine (Int) (Sum Int) -> Bool)

-- *MonoidsSemigroups> f = Combine $ \n -> Sum (n + 1)
-- *MonoidsSemigroups> g = Combine $ \n -> Sum (n - 1)
-- *MonoidsSemigroups> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- *MonoidsSemigroups> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- *MonoidsSemigroups> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- *MonoidsSemigroups> unCombine (g <> f) $ 1
-- Sum {getSum = 2}


-- smVal1 = unCombine (fCmb <> gCmb) $ 0

-- instance  Semigroup (Combine a b ) where
--     Combine (a -> b)  <> Combine (a' -> b')   =   Combine (a -> b')
-- -- instance  Semigroup (Combine a b ) where
-- --     unCombine :: (a -> b)  <> unCombine (Combine (a' b'))  = (unCombine ((Combine a b)))  UnCombine (Combine a' b')



-- 10. newtype Comp a =
-- Comp { unComp :: (a -> a) }
-- Hint: We can do something that seems a little more specific and natural to functions now that the input and
-- output types are the same.
newtype Comp a =
    Comp { unComp :: (a -> a) } 
-- instance Semigroup (Comp a ) where 
--   Comp {unComp=f} <> Comp {unComp=g} = Comp f

instance Show (Comp a) where      -- can show a function so you should intialize something random so it does not complain of lack of Show
    show f = "Unicorns!!"

instance (Semigroup a) => Semigroup (Comp a) where
    (Comp fx) <> (Comp fy) = Comp (fx . fy)


instance Monoid a => Monoid (Comp a) where
        mempty = Comp {unComp=f} where 
                     f a = mempty

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


-- *MonoidsSemigroups> :t fComposition1
-- fComposition1 :: Integer -> Integer

fFunc1 = \n -> (n + 1)
fFunc2 = \n -> (n + 1)
fComposition1 = fFunc1.fFunc2
-- *MonoidsSemigroups> fComposition1 5
-- 7
-- *MonoidsSemigroups> :t fComposition1
-- fComposition1 :: Integer -> Integer

fCurry1 i = (fFunc1) ((fFunc2) i) 
-- *MonoidsSemigroups> :t fCurry1
-- fCurry1 :: Integer -> Integer



fSumFunc1 :: Sum Int -> Sum Int
fSumFunc1 (Sum n) = Sum n + Sum 1

fSumFunc2 :: Sum Int -> Sum Int
fSumFunc2 (Sum n) = Sum n + Sum 2


-- smValCmp = fCmp3 <> fCmp4
valFCompFunc1 = fSumFunc1 (Sum 25)
-- *MonoidsSemigroups> valFCompFunc1
-- Sum {getSum = 26}

fComp1 :: Comp (Sum Int)
fComp1 = Comp (fSumFunc1)

fComp2 :: Comp (Sum Int)
fComp2 = Comp (fSumFunc2)
fCompMempty :: Comp (Sum Int)
fCompMempty = mempty

fComp3 = fComp1 <> fComp2
valFComp3 = (unComp fComp3) (Sum 47)

fMemptyComp1Right = fComp1 <> fCompMempty
fMemptyComp1Left =  fCompMempty <> fComp1
valFMemptyComp1Right = (unComp (fMemptyComp1Right)) (Sum 5)
valFMemptyComp1Left = (unComp (fMemptyComp1Left)) (Sum 5)

-- *MonoidsSemigroups> valFComp3
-- Sum {getSum = 50}
-- type CompSumIntTest =
--      (Comp (Sum Int -> Sum Int)) -> (Comp (Sum Int -> Sum Int)) -> (Comp (Sum Int -> Sum Int)) -> Bool
type CompSumIntTest =
     Sum Int -> Comp (Sum Int)  -> Comp (Sum Int) -> Comp (Sum Int) -> Bool

compAssoc :: (Semigroup a, Eq a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc v a b c = (unComp (a <> (b <> c)) $ v) == (unComp ((a <> b) <> c) $ v)


compAssocSumInt = quickCheck ( compAssoc :: CompSumIntTest)

-- compSemigroupAssoc :: (Sum Int -> Sum Int) -> (Sum Int -> Sum Int) -> (Sum Int -> Sum Int) -> Bool
-- compSemigroupAssoc (Fun _ f1) (Fun _ f2) (Fun _ f3) =
--     (Comp (f1) <> (Comp (f2) <> Comp (f3))) == ((Comp (f1) <> Comp (f2)) <> Comp (f3))


-- semigroupAssoc :: (Eq m, Semigroup m)
--     => m -> m -> m -> Bool
-- semigroupAssoc a b c =
--     (a <> (b <> c)) == ((a <> b) <> c)


-- f3 :: Comp (Int -> Int)
-- f3 = fCmp1 <> fCmp2

-- type CompIntTest =
--     (Comp (Int -> Int)) -> (Comp (Int -> Int)) -> Int -> Int
-- smvalUnCompfCmp1 :: CompIntTest
-- smvalUnCompfCmp1 c1 c2 i = (unComp ((c1 <> c2))) i

-- valCmp = unComp fCmp2 ((unComp fCmp1) 5 )

-- valCmp1 = unComp(fCmp1 <> fCmp2) $ 1


-- Shrinking and showing Functions Using QuickCheck
-- https://www.youtube.com/watch?v=CH8UQJiv9Q4

-- see /Users/chakravartiraghavan/Documents/Typora1/Blockchain/CardanoTraining/Haskell/Projects/projectTesting/MapFilter.hs


-- 11. Look familiar?
-- data Validation a b =
-- Failure a | Success b
-- deriving (Eq, Show)
-- instance Semigroup a =>
-- Semigroup (Validation a b) where
-- (<>) = undefined
-- main = do
-- let failure :: String
-- -> Validation String Int
-- failure = Failure
-- success :: Int
-- -> Validation String Int
-- success = Success
-- print $ success 1 <> failure "blah"
-- print $ failure "woot" <> failure "blah"
-- print $ success 1 <> success 2
-- print $ failure "woot" <> success 2
-- You should get this output:
-- Prelude> main
-- Success 1
-- Failure "wootblah"
-- Success 1
-- Success 2

data Validation a b =
    Failure' a | Success' b
    deriving (Eq, Show)

instance Semigroup a =>
    Semigroup (Validation a b) where
    Failure' a <> Failure' a' = Failure' (a <> a')
    Success' b <> Success' b' = Success' b
    Success' b <> Failure' a = Success' b
    Failure' a <> Success' b = Success' b

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b) 
genValidation = do 
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure' a,
           return $ Success' b] 


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = genValidation

type ValidationStringInt =
    (Validation String Int) -> (Validation String Int) -> (Validation String Int) -> Bool

validationAssocStringInt = quickCheck ( semigroupAssoc :: ValidationStringInt)

-- Success' 1
-- Failure' "wootblah"
-- Success' 1
-- Success' 2


-- using stimes from Semigroup
-- *MonoidsSemigroups> stimes 4 [1]
-- [1,1,1,1]
-- *MonoidsSemigroups> stimes 4 (Sum 25)
-- Sum {getSum = 100}

-- Monoid exercises
-- did it with the semigroup instances since it was same data types


-- Ex 8. This next exercise will involve doing something that will still feel a bit unnatural, and you may find it difficult. If
-- you get it, and you haven’t done much FP or Haskell before, get yourself a nice beverage. We’re going to toss
-- you the instance declaration, so you don’t churn on a missing Monoid constraint you didn’t know you need:
-- newtype Mem s a =
-- Mem {
-- runMem :: s -> (a,s)
-- }
-- instance Semigroup a => Semigroup (Mem s a) where
-- (<>) = undefined
-- instance Monoid a => Monoid (Mem s a) where
-- mempty = undefined
-- Given the following code:
-- f' = Mem $ \s -> ("hi", s + 1)
-- main = do
-- let rmzero = runMem mempty 0
-- rmleft = runMem (f' <> mempty) 0
-- rmright = runMem (mempty <> f') 0
-- print $ rmleft
-- print $ rmright
-- print $ (rmzero :: (String, Int))
-- print $ rmleft == runMem f' 0
-- print $ rmright == runMem f' 0

-- A correct Monoid for Mem should, given the above code,
-- produce the following output:
-- Prelude> main
-- ("hi",1)
-- ("hi",1)

-- ("",0)
-- True
-- True


-- Make certain your instance has output like the above, as this is sanity checking the Monoid identity laws for you! It’s
-- not a proof, and it’s not even as good as property testing, but it’ll catch the most common mistakes people make.
-- It’s not a trick, and you don’t need a Monoid for s. Yes, such a Monoid can and does exist. Hint: chain the s values
-- from one function to the other. You’ll want to check the identity laws, as a common first attempt will break them.

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Show (Mem s a) where      -- can show a function so you should intialize something random so it does not complain of lack of Show
    show f = "Unicorns!!"

instance Semigroup a => Semigroup (Mem s a) where
    Mem {runMem = f } <> Mem {runMem = g } = Mem { runMem = h} where
                        h s = (( fst (f s) <> fst (g s) ), snd (f (snd (g s))))

instance Monoid a => Monoid (Mem s a) where
        mempty = Mem {runMem=f} where 
                        f s = (mempty, s)


f' = Mem $ \s -> ("hi", s + 1)
rmzero = runMem ( mempty :: Mem Int String) 0
rmleft = runMem (f' <> mempty) 0
rmright = runMem (mempty <> f') 0

-- *MonoidsSemigroups> rmleft
-- ("hi",1)
-- *MonoidsSemigroups> rmright
-- ("hi",1)
-- *MonoidsSemigroups> rmzero
-- ("",0)
-- *MonoidsSemigroups> rmleft == runMem f' 0
-- True
-- *MonoidsSemigroups> rmright == runMem f' 0
-- True


-- snd ((runMem (Mem {runMem = f }) s)) <> snd ((runMem (Mem {runMem = g }) s)) =    
--                   where                     
--                       g s = ( (a' :: a) , s' :: s)
--                       f s = ( a, s ) 
--                       f s' = ( _ , s'')
--                       h'' s = ( ( a<>a'), s'' )


-- (runMem (f' <> f2') ) 5 =     (s <> s', 10) == runMem ()


-- instance Monoid a => Monoid (Mem s a) where
-- mempty = undefined

-- instance (Semigroup b) 
--   => Semigroup (Combine a b) where 
--   Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)



f2' = Mem $ \s -> (" Rafa", s + 4)

-- *MonoidsSemigroups> (runMem (f' <> f2') 24)
-- ("hi Rafa",29)

f'Val1 = (runMem (f')) 5
-- *MonoidsSemigroups> f'Val1
-- ("hi",6)
-- *MonoidsSemigroups> "hi" <> " Rafa"
-- "hi Rafa"


--- techincally for mappend i want - 
-- (runMem (f' <> f2') ) 5 =     ("hi Rafa", 10) == runMem ()


aTup1 = (1,2)


main :: IO ()
main = do
    putStrLn (" m1 mappend = " ++ show(m1))
    putStrLn (" m2 mappend mempty = " ++ show(m2))
    putStrLn (" m3 mappend mempty = " ++ show(m3))
    putStrLn (" mc1 moncat = " ++ show(mc1))
    let g = " goes well with garlic"
    putStrLn (" mc1 moncat = " ++ show(mappend "Trout" g))
    putStrLn (" ++ = " ++ show(plusplus))
    putStrLn (" foldr ++ = " ++ show(f))
    putStrLn (" foldr mappend = " ++ show(f2))
    putStrLn (" sum = " ++ show(sum1))
    putStrLn (" prod1 = " ++ show(prod1))
    putStrLn (" sum3 3 variables = " ++ show(sum3))
    putStrLn (" sum4 <> operator = " ++ show(sum4))
    putStrLn (" mconcat over sum = " ++ show(mcon1))
    putStrLn (" getsum = " ++ show(getsum1))
    putStrLn (" getProduct = " ++ show(getprod1))
    putStrLn (" getsum concat w $ = " ++ show(getsum3))
    putStrLn (" getsum concat w  brackets = " ++ show(getsum4))
    putStrLn (" prod int declare and fold = " ++ show(fol1))
    putStrLn (" sum int declare and fold = " ++ show(sumfol1))
    putStrLn (" String and fold = " ++ show(strListFold))
    putStrLn (" Left Identity = " ++ show(leftI))
    putStrLn (" Right Identity = " ++ show(rightI))
    putStrLn (" Bool1  = " ++ show(bool1))
    putStrLn (" Bool2  = " ++ show(bool2))
    putStrLn (" Bool3  = " ++ show(bool3))
    putStrLn (" Bool4  = " ++ show(bool4))
    putStrLn (" First1  = " ++ show(first1))
    putStrLn (" First with 1st Nothing and 2nd Just = " ++ show(first2))
    foo 50
    foo 1000   -- Does not print anything 
    putStrLn (" Opt1  = " ++ show(opt1))
    putStrLn ("ProdOpt1  = " ++ show(prodOpt1))
    putStrLn ("sumNada  = " ++ show(sumNada))
    putStrLn ("sum1Nada  = " ++ show(sum1Nada))
    putStrLn ("sum1_2  = " ++ show(sum1_2))
    putStrLn ("madlib  = " ++ show(madlib1))
    quickCheck (monoidAssoc :: MA)
    quickCheck (mli :: String -> Bool)
    quickCheck (mri :: String -> Bool)
    print (" Monoid Left Identity test: Trivial ")
    quickCheck (trvMli :: Trivial -> Bool)
    print (" Monoid right Identity test: Trivial ")
    quickCheck (trvMlr :: Trivial -> Bool)
    let ma = monoidAssoc
        mli1 = monoidLeftIdentity
        mri1 = monoidRightIdentity
    putStrLn (" Quickcheck  ma :: BullMappend : ")
    quickCheck (ma :: BullMappend)
    putStrLn (" mli1 :: Bull -> Bool : ")
    quickCheck (mli1 :: Bull -> Bool)
    putStrLn (" mli1 :: Bull -> Bool: ")
    quickCheck (mri1 :: Bull -> Bool)
    putStrLn ("prodNE3 =  1 :| [2,3,4] <> 5 :| [6,7,8]   =   " ++ show(prodNE3))
    putStrLn ("prodNE3 head   =   " ++ show(headProdNE3))
    putStrLn ("prodNE3 tail   =   " ++ show(tailProdNE3))
    putStrLn ("prodNE3 length   =   " ++ show(lenProdNE3))
    quickCheck (semigroupAssoc :: TrivAssoc)
    print (" Monoid Assoc FirstMappend test: ")
    quickCheck (monoidAssoc :: FirstMappend)
    print (" Monoid Left Identity test: FstId ")
    quickCheck (monoidLeftIdentity :: FstId)
    print (" Monoid Right Identity test: FstId ")
    quickCheck (monoidRightIdentity :: FstId)
    print (" SemiGroup Trivial associativity test:  ")
    quickCheck (semigroupAssoc :: TrivAssoc)
    print (" SemiGroup Identity a associativity test: Sum Int")
    quickCheck ( semigroupAssoc :: IdAssocSumInt)
    print (" SemiGroup Identity a associativity test: String")
    quickCheck ( semigroupAssoc :: IdAssocString)
    print (" SemiGroup Two a b associativity test: Sum Int")
    quickCheck ( semigroupAssoc :: TwoAssocSumInt)
    print (" SemiGroup Two a b associativity test: Prod Int")
    quickCheck ( semigroupAssoc :: TwoAssocProdInt)
    print (" SemiGroup Three a b c associativity test: Sum Int")
    quickCheck ( semigroupAssoc :: ThreeAssocSumInt)
    print (" SemiGroup Three a b c associativity test: Product Int")
    quickCheck ( semigroupAssoc :: ThreeAssocMaybeSumInt)
    print (" SemiGroup Or a b  associativity test: Int")
    quickCheck ( semigroupAssoc :: OrInt)
    print (" SemiGroup Validation a b  associativity test: String Int")
    quickCheck ( semigroupAssoc :: ValidationStringInt)
    let failure :: String
            -> Validation String Int
        failure = Failure'
        success :: Int
            -> Validation String Int
        success = Success'
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
    
    -- putStrLn s
    -- sgIdAssocSumInt
    -- verboseCheck monoidAssoc
    -- putStrLn ("QuickCheck associativity  = " ++ show(quickcheck1))
    -- if first3 == First {getFirst = Nothing}
    --   then return ()
    --   else putStrLn (" Success ")
    -- putStrLn (" First with both Nothing = " ++ show(first3))       -- Errors 
   -- print Nothing -- ERrors 