
module Purplebook where 

-- import Data.Semigroup as S     -- Since Data.Monid is already imported seems this one is not needed or creating conflict
import Data.List.NonEmpty as N
import Data.Monoid hiding ((<>))
import Data.List as L

import Test.QuickCheck
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

-- Gave error w/o the Semi group.Below link gave solution
-- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

onlySum = Only (Sum 1)
opt1 = onlySum `mappend` onlySum
-- *Purplebook> opt1
-- Only (Sum {getSum = 2})
onlyFour = Only (Product 4)
onlyTwo = Only (Product 2)
prodOpt1 = onlyFour `mappend` onlyTwo

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
type B = Bool
type MA = S -> S -> S -> B

quickcheck1 :: IO ()
quickcheck1 = quickCheck (monoidAssoc :: MA)   -- +++ OK, passed 100 tests.
-- *Purplebook> quickcheck1 
-- +++ OK, passed 100 tests.

type S' = Sum Int
type MA' = S' -> S' -> S' -> B
quickcheck2 = quickCheck (monoidAssoc :: MA')
-- *Purplebook> quickcheck2
-- +++ OK, passed 100 tests.

type Bl = BlockChain Integer 
type MABl = Bl -> Bl -> Bl -> B
quickcheck3 = quickCheck (monoidAssoc :: MABl)
-- *Purplebook> quickcheck3
-- +++ OK, passed 100 tests.

-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Eq.html
-- The Eq class defines equality (==) and inequality (/=). All the basic datatypes exported by the Prelude are instances of Eq, 
--     and Eq may be derived for any datatype whose constituents are also instances of Eq.


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

instance Monoid Bull where
    mempty = Fools


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


instance Arbitrary a =>
        Arbitrary (First' a) where
    arbitrary =
        frequency [(1, return Nothing),
                  (3, liftM Just arbitrary)]


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

prodNE1 = 1 :| [2,3,4]
prodNE2 = 5 :| [6,7,8]
prodNE3 = prodNE1 <> prodNE2
-- prodElm1 = head 
headProdNE3 = N.head prodNE3
tailProdNE3 = N.tail prodNE3
lenProdNE3 = N.length prodNE3


-- Prelude Data.List.NonEmpty Data.Semigroup> :t 1 :| [2, 3]
-- 1 :| [2, 3] :: Num a => NonEmpty a



-- https://wiki.haskell.org/Data.Semigroup

-- 15.15 Chapter exercises

-- Semigroup exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = undefined


instance Arbitrary Trivial where
    arbitrary = return Trivial


semigroupAssoc :: (Eq m, Semigroup m)
    => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool
    


-- https://stackoverflow.com/questions/39232294/how-to-write-a-semigroup-instance




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

    -- verboseCheck monoidAssoc
    -- putStrLn ("QuickCheck associativity  = " ++ show(quickcheck1))
    -- if first3 == First {getFirst = Nothing}
    --   then return ()
    --   else putStrLn (" Success ")
    -- putStrLn (" First with both Nothing = " ++ show(first3))       -- Errors 
   -- print Nothing -- ERrors 

    





