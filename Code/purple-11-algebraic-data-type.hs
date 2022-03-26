{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Int
import Data.Char
import Data.List

-- {-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, FunctionalDependencies #-}

import Data.Char


f x = x > 3


data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = 
    DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10


data Doggies a =
    Husky a
    | Mastiff a
    deriving (Eq, Show)


-- Exercises: Dog types
-- Given the datatypes defined in the above sections:

-- 1. Is Doggies a type constructor or a data constructor?    -- Its a type constructor
-- 
-- 2. What is the kind of Doggies?       -- *->*      
-- *Main> :k Doggies 
-- -- Doggies :: * -> *


-- 3. What is the kind of Doggies String?    -- Since already 'a' is applied its now * 
-- *Main> :k Doggies String
-- Doggies String :: *


-- 4. What is the type of Husky 10?       -- 10 is Num and noy yet concretized withun Num class.
-- *Main> :t Husky 10
-- Husky 10 :: Num a => Doggies a


-- 5. What is the type of Husky (10 :: Integer)?     -- should be Integer a => Doggies Integer
-- *Main> :t Husky (10 :: Integer)
-- Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?   -- since "Scooby Doo" resolves to String. It will be Doggies String or [Char]
-- *Main> :t Mastiff "Scooby Doo"
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor? --- its both

-- 8. What is the type of DogueDeBordeaux?
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- *Main> :t DogueDeBordeaux 
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie!" :: DogueDeBordeaux String
-- *Main> :t DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]



data Price =
    Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)


data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
    | Plane Airline
    | Plane' Airline AirlineSize
    deriving (Eq, Show)

-- Exercises: Vehicles

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

plane1 = Plane' TakeYourChancesUnited Airbus
plane2 = Plane' CatapultsR'Us Boing747

-- 1. What is the type of myCar?    -- myCar :: Vehicle 
-- *Main> :t myCar
-- myCar :: Vehicle


-- 2. Given the following, define the functions:

isCar :: Vehicle -> Bool
-- isCar = undefined
isCar (Car _ _) =  True 
isCar (Plane _) = False
isCar (Plane' _  _) = False


isPlane :: Vehicle -> Bool
-- isPlane = undefined
isPlane (Car _ _) =  False
isPlane (Plane _) = True
isPlane (Plane' _ _) = True

areCars :: [Vehicle] -> [Bool]
-- areCars = undefined
areCars [] = []
areCars [x] = [isCar x]
areCars (x:xs) = isCar x : areCars xs

-- *Main> isCar myCar
-- True
-- *Main> isCar urCar
-- True
-- *Main> isCar clownCar
-- True
-- *Main> isCar doge
-- False
-- *Main> isPlane doge
-- True
-- *Main> isPlane myCar
-- False
-- *Main> areCars [myCar, urCar, clownCar, doge]
-- [True,True,True,False]

-- 3. Now, we’re going to write a function to tell us the manufacturer of a piece of data:
getManu :: Vehicle -> Manufacturer
-- getManu = undefined
getManu (Car x _) = x
getManu (Plane x) = error "No manufacturer for Plane"
getManu (Plane' x _) = error "No manufacturer for Plane"

-- 5. All right. Let’s say you decide to add the size of the plane as an argument to the Plane constructor. Add that to your
-- datatypes in the appropriate places, and change your data and functions appropriately.

data AirlineSize = Boing747 | Airbus | Jet | SmallJet deriving (Eq, Show)


-- *Main> isPlane plane1
-- True
-- *Main> isPlane doge
-- True

-- *Main> areCars [myCar, urCar, clownCar, doge, plane1, plane2]
-- [True,True,True,False,False,False]



-- 11.7 Data constructor arities

--  nullary
data Example0 =
    Example0
    deriving (Eq, Show)

--  unary
data Example1 =
    Example1 Int
    deriving (Eq, Show)
    
--  product of Int and String
data Example2 =
    Example2 Int String
    deriving (Eq, Show)


-- 11.8 What makes these datatypes algebraic?

-- Exercises: Cardinality

-- 1. 
-- data PugType = PugData      -- cardinality 1 - only 1 possible data value

-- 2. 
-- data Airline =
--     PapuAir
--     | CatapultsR'Us
--     | TakeYourChancesUnited
--     deriving (Eq, Show)       -- cardinality is 3 -  only 3 possible data values 

-- 3. Given what we know about Int8, what’s the cardinality of
-- Int16?

-- Prelude> import Data.Int
-- Prelude Data.Int> minBound :: Int8
-- -128
-- Prelude Data.Int> maxBound :: Int8
-- 127
-- the cardinality of Int8 with some quick addition: 128 + 127 + 1 = 256.
-- Prelude Data.Int> minBound :: Int16
-- -32768
-- Prelude Data.Int> maxBound :: Int16
-- 32767
-- cardiality is 65536 for Int16


-- 4. Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?

-- Prelude Data.Int> maxBound :: Int
-- 9223372036854775807
-- Prelude Data.Int> minBound :: Int
-- -9223372036854775808
-- cardinality of int is 18446744073709551616

-- Prelude Data.Int> minBound :: Integer

-- <interactive>:8:1: error:
--     • No instance for (Bounded Integer)
--         arising from a use of ‘minBound’
--     • In the expression: minBound :: Integer
--       In an equation for ‘it’: it = minBound :: Integer


-- 5. Extra credit (impress your friends!): what’s the connection between the 8 in Int8 and that type’s cardinality of 256?
-- 8 digits so each can be 0 or 1 - so total possible values:
-- Prelude Data.Int> 2 ^ 8
-- 256




-- Exercises: For example

data Example = MakeExample deriving Show

-- Ex 1
-- What is the type of the data constructor MakeExample? What happens when you request the type of Example?

-- MakeExample will be of type Example
-- *Main> :t MakeExample
-- MakeExample :: Example

-- *Main> :t Example

-- <interactive>:1:1: error:
--     • Data constructor not in scope: Example




-- Ex 2 
-- What if you try :info on Example in GHCi? Can you determine what type class instances are defined for the Example type using :info in GHCi?

-- *Main> :i Example
-- type Example :: *
-- data Example = MakeExample
--   	-- Defined at purple-11-algebraic-data-type.hs:245:1
-- instance [safe] Show Example
--   -- Defined at purple-11-algebraic-data-type.hs:245:37

-- Ex 3 - 

-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. 
-- What has changed when you query MakeExample with :type in GHCi?


data Example' = MakeExample' Int deriving Show
-- *Main> :t MakeExample'
-- MakeExample' :: Int -> Example'
 


-- 11.9 newtype

class TooMany a where
    tooMany :: a -> Bool
    sumIt :: a -> Int


instance TooMany Int where
    tooMany n = n > 42
    sumIt a = a

-- *Main> tooMany (42 :: Int)
-- False
-- *Main> tooMany (44 :: Int)
-- True
-- *Main> tooMany 40


instance TooMany Char where
    tooMany a = a > 'q' 
    sumIt a = ord a
    -- tooMany (a,[]) = a > 42
    -- tooMany (a,[b]) = a > 42 && b > 'q'

instance TooMany (Int, Char) where
    tooMany (a,b) = a > 42 && b > 'q'
    sumIt (a,b) = a + (ord b)

instance TooMany (Int, Int) where
    tooMany (a,b) = a > 42 && b > 42
    sumIt (a,b) = a + b

instance TooMany (Int, [Char]) where
    tooMany (a,(b:bs)) = a > 42 && b > 'q'
    sumIt (a, b)  = foldr (+) 0 ( map ord b) 

-- <interactive>:26:1: error:
--     • Ambiguous type variable ‘a0’ arising from a use of ‘tooMany’
--       prevents the constraint ‘(TooMany a0)’ from being solved.

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = n > 43
    sumIt (Goats n) = n

-- *Main> tooMany (Goats 40)
-- False
-- *Main> tooMany (Goats 45)
-- True

-- *Main> :t tooMany 
-- tooMany :: TooMany a => a -> Bool



newtype Goats' =
    Goats' Int deriving (Eq, Show)

instance TooMany Goats' where
    tooMany (Goats' n) = tooMany n
    sumIt (Goats' n) = sumIt n

-- *Main> tooMany (Goats' 45)
-- True
-- *Main> tooMany (Goats' 40)
-- False

newtype Goats'' =
    Goats'' Int deriving (Eq, Show, TooMany)

-- *Main> tooMany (Goats'' 40)
-- False
-- *Main> tooMany (Goats'' 45)
-- True

newtype Boats = Boats (Int, [Char]) deriving (Eq, Show, TooMany)

-- Exercises: Logic goats


-- 1. Reusing the TooMany type class, write an instance of the type class for the type (Int, String). This will require
-- adding a language pragma named FlexibleInstances4 if you do not use a newtype—GHC will tell you what to do.

-- *Main> tooMany (Boats (47,"serguy"))
-- True

-- *Main> tooMany (Boats (47,"erguy"))
-- False

-- *Main> tooMany (Boats (7,"zerguy"))
-- False
-- *Main> tooMany (Boats (7,"erguy"))
-- False
-- *Main> tooMany (Boats (79,"werguy"))
-- True

-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption that this is a count of goats from two fields

newtype SumGoats = SumGoats (Int,Int) deriving (Eq, Show, TooMany)
-- *Main Data.Char> sumIt (SumGoats (3,9))
-- 12

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such as summing the two numbers together

-- instance TooMany' (Num a, TooMany Int) where
--     tooMany (a, b) = a > b 
--     sumIt (a, b)  =  a + b

class ThreeTooMany' a  where
    addit' :: a -> Int

instance ThreeTooMany' (Int, Int) where
    addit' (a,b) = a + b   --  + sumIt b


-- class ThreeTooMany a  where
--     addit :: Num b => a -> b
-- instance (Num a) => ThreeTooMany a where
--     addit a  = a   --  Does not compile
--     -- addit (a,b) = 10   --  Compiles fine

-- tooMany.hs


-- newtype SampleBin = SampleBin Int Int deriving (Eq, Show)    -- will error since its Unary data Constructor cant have 2 data constructors



-- 11.10 Sum types

-- Exercises: Pity the Bool

-- 1

data BigSmall =
    Big Bool
    | Small Bool
    deriving (Eq, Ord, Show)


-- 2. Given a datatype:
-- bring Int8 in scope

data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

myNumba = Numba (-128)

-- What is the cardinality of NumberOrBool? What happens if you try to create a Numba with a numeric literal larger than
-- 127? And with a numeric literal smaller than -128?
-- If you choose (-128) for a value precisely, you’ll notice you get a spurious warning:
-- Prelude> n = Numba (-129)
-- Literal -129 is out of the
-- Int8 range -128..127

-- *Main> maxBound::Int8
-- 127
-- *Main> minBound::Int8
-- -128
-- 256 is caridnality of Numba + 2 for BoolyBool = 258.

-- *Main> Numba 128

-- <interactive>:36:7: warning: [-Woverflowed-literals]
--     Literal 128 is out of the Int8 range -128..127
--     If you are trying to write a large negative literal, use NegativeLiterals
-- Numba (-128)

-- *Main> Numba (-130)

-- <interactive>:39:9: warning: [-Woverflowed-literals]
--     Literal -130 is out of the Int8 range -128..127
-- Numba 126


-- 11.12 Normal form

data Fiction = Fiction deriving Show                 -- basically you creating new type and also new variable - type and type contructor
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
    | NonfictionBook Nonfiction
    deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

data Author' =
    Fiction' AuthorName
    | Nonfiction' AuthorName
    deriving (Eq, Show)



-- Exercises: How does your garden grow?
-- 1. Given the type:
data FlowerType = Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String
data Garden =
    Garden Gardener FlowerType
    deriving Show



-- What is the sum of products normal form of Garden? Exercises: How does your garden grow?

data Garden2 = 
    Daisy' Gardener                  -- you need Daise' otherwise its like declaring a variable twice. As Daisy already was declared as a FlowerType
    | Rose' Gardener
    | Gardenia' Gardener 
    | Lilac' Gardener 
    deriving Show


g1 = Garden "Rafa" Daisy             -- here you assign the variable
g2 = Garden "Djoker" Lilac
g3 = Garden "Fed" Rose

g4 = Daisy' "Rafa"
g5 = Lilac' "Djoker"
g6 = Rose' "Fed"



-- 11.13 Constructing and deconstructing values

data GuessWhat =
    ChickenButt deriving (Eq, Show)
data Id a =
    MkId a deriving (Eq, Show)
data Product a b =
    Product a b deriving (Eq, Show)
data Sum a b =                                      -- means these can 2 different types. if you have "a a" instead then once you use a type other has to be same.
    First a
    | Second b
    deriving (Eq, Show)
data RecordProduct a b =
    RecordProduct { pfirst :: a
    , psecond :: b }
    deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

cow1 = NumCow 1
cow2 = NumCow 2

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)
pig1 = NumPig 1
pig2 = NumPig 2


data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

fh1 = Farmhouse cow1 pig1

prod1fh = Product cow2 pig1 

newtype NumSheep =
    NumSheep Int 
    deriving (Eq, Show)

sheep1 = NumSheep 10

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)
type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)
data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)
data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

-- Alternatively
type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)

bess' = (CowInfo "Bess" 4)
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'


sheep = SheepInfo "Baaaaa" 5 5
-- *Main> :t First (Second sheep)
-- First (Second sheep) :: Sum (Sum a SheepInfo) b

-- *Main> :info Animal'
-- type Animal' :: *
-- type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
--     -- Defined at purple-11-algebraic-data-type.hs:602:1


-- Constructing values


trivialValue :: GuessWhat
trivialValue = ChickenButt


-- data Id a =
--     MkId a deriving (Eq, Show)
-- Because Id has an argument, we have to apply it to something before we can construct a value of that type:

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
    Twitter deriving (Eq, Show)

data AskFm =
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- alternative- We can assert that ordering directly by writing a datatype like this:
data SocialNetwork' =
    Twitter'
    | AskFm'
    deriving (Eq, Show)


data PPoint a = PPoint a a
    deriving (Show, Eq)



-- Exercise: Programmers

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
    , lang :: ProgLang }
    deriving (Eq, Show)

-- *Main> :t Programmer
-- Programmer :: OperatingSystem -> ProgLang -> Programmer


nineToFive :: Programmer
nineToFive = Programmer { os = Mac
    , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly =
    Programmer { lang = Agda
    , os = GnuPlusLinux }

-- Write a function that generates all possible values of Programmer. Use the provided lists of inhabitants of OperatingSystem and ProgLang:


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = allProgFunc allOperatingSystems allLanguages


allProgFunc :: [OperatingSystem] -> [ProgLang] -> [Programmer]
allProgFunc [] [] = []
allProgFunc [x] [y] = [Programmer { os = x, lang = y}]
allProgFunc [x] [] = []
allProgFunc [] [y] = []
allProgFunc (x:xs) [y] =  Programmer { os = x, lang = y} : allProgFunc xs [y] 
allProgFunc [x] (y:ys) = Programmer { os = x, lang = y} : allProgFunc [x] ys
allProgFunc (x:xs) (y:ys) =  (Programmer { os = x, lang = y} : allProgFunc xs [y] ) ++ allProgFunc (x:xs) ys

-- *Main> allProgrammers 
-- [Programmer {os = GnuPlusLinux, lang = Haskell},Programmer {os = OpenBSDPlusNevermindJustBSDStill, lang = Haskell},Programmer {os = Mac, lang = Haskell},
--      Programmer {os = Windows, lang = Haskell},Programmer {os = GnuPlusLinux, lang = Agda},Programmer {os = OpenBSDPlusNevermindJustBSDStill, lang = Agda},
--      Programmer {os = Mac, lang = Agda},Programmer {os = Windows, lang = Agda},Programmer {os = GnuPlusLinux, lang = Idris},
--      Programmer {os = OpenBSDPlusNevermindJustBSDStill, lang = Idris},Programmer {os = Mac, lang = Idris},Programmer {os = Windows, lang = Idris},
--      Programmer {os = GnuPlusLinux, lang = PureScript},Programmer {os = OpenBSDPlusNevermindJustBSDStill, lang = PureScript},
--      Programmer {os = Mac, lang = PureScript},Programmer {os = Windows, lang = PureScript}]
-- *Main> length allProgrammers 
-- 16
-- *Main> length allOperatingSystems * length allLanguages
-- 16


-- Accidental bottoms from records

data ThereYet =
    There Float Int Bool
    deriving (Eq, Show)

nope = There

-- Who needs a "builder pattern"?
notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

myNope :: Float -> Int -> Bool -> ThereYet
myNope = There
-- *Main> myNope 14.3 3 True
-- There 14.3 3 True


-- Deconstructing values
-- newtype Name = Name String deriving Show   -- already declared
newtype Acres = Acres Int deriving Show


-- FarmerType is a Sum
data FarmerType = DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving Show

-- Farmer is a plain old product of
-- Name, Acres, and FarmerType
data Farmer =
    Farmer Name Acres FarmerType
    deriving Show


isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) =
    True
isDairyFarmer _ =
    False


-- using record syntax  
data FarmerRec =
    FarmerRec { name :: Name
    , acres :: Acres
    , farmerType :: FarmerType }
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
      DairyFarmer -> True
      _ -> False



-- 11.14 Function type is exponential


data Quantum =
    Yes
    | No
    | Both
    deriving (Eq, Show)

-- Exponentiation in what order?
-- Consider the following function:
--convert :: Quantum -> Bool
-- convert = undefined
-- According to the equality of a -> b and ba , there should be 2^3 or 8 implementations of this function. Does this hold?
-- Write it out, and prove it for yourself.

convert1 ::  Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True


convert3 ::  Quantum -> Bool
convert3 Yes = True
convert3 No = True
convert3 Both = False

convert4 ::  Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = True


convert2 ::  Quantum -> Bool
convert2 Yes = True
convert2 No = False
convert2 Both = False



convert5 ::  Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True


convert6 ::  Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 ::  Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True


convert8 ::  Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False




-- Exercises: The Quad

-- Determine how many unique inhabitants each type has. Suggestion: do the arithmetic, unless you want to verify.
-- Writing them out gets tedious quickly:

-- 1. 
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- since its Sum type constants it is 4.

-- How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = Left One

-- since this is product type 
-- type Either :: * -> * -> *
-- data Either a b = Left a | Right b

-- should be 8 since Left Quad has 4, Right quad can be 4. So 4+4 = 8

-- 2. 
prodQuad :: (Quad, Quad)
prodQuad = (One, One)      
prodQuad1 = (One, Two)    --- etc etc
-- there can be 4 x 4 combinations = 16

-- 3. 
funcQuad :: Quad -> Quad
funcQuad One = One 
funcQuad Two = One
funcQuad Three = One
funcQuad Four = One

funcQuad1 :: Quad -> Quad
funcQuad1 One = Two 
funcQuad1 Two = One
funcQuad1 Three = One
funcQuad1 Four = One
-- etc etc.. 

-- func a->b is b^a. so there it is 4^4 = 64. 

-- 4. 
-- prodTBool :: (Bool, Bool, Bool)

-- -- Products are 2 * 2 * 2 = 8  (bool its True or False)
-- true true true
-- true true False
-- true False true
-- True False False    ... similary another 4

-- 5. gTwo :: Bool -> Bool -> Bool
-- function a -> b -> c is : 
-- ( c ^ b ) ^ a
-- but there they are all same kind so its a product type 2 * 2 * 2 = 8

-- 6. Hint: five digit number
-- fTwo :: Bool -> Quad -> Quad

-- (4 ^ 4) ^ 2 = 256 ^ 2 = 65536


-- 11.15 Higher-kinded datatypes

-- identical to (a, b, c, d)
data Silly a b c d =
    MkSilly a b c d deriving Show


-- *Main> :kind Silly
-- Silly :: * -> * -> * -> * -> *
-- *Main> :kind Silly Int
-- Silly Int :: * -> * -> * -> *
-- *Main> :kind Silly Int String
-- Silly Int String :: * -> * -> *
-- *Main> :kind Silly Int String Bool
-- Silly Int String Bool :: * -> *
-- *Main> :kind (,,,,)
-- (,,,,) :: * -> * -> * -> * -> * -> *
-- *Main> :kind (Int, String, Bool, String)
-- (Int, String, Bool, String) :: *


-- data EsResultFound a =
--     EsResultFound
--     { _version :: DocVersion
--     , _source :: a
--     } deriving (Eq, Show)

-- instance (FromJSON a) =>
--     FromJSON (EsResultFound a) where
--         parseJSON (Object v) =
--             EsResultFound
--           <$> v .: "_version"
--           <*> v .: "_source"
--         parseJSON _ = empty


-- 11.16 Lists are polymorphic


-- data [] a = [] | a : [a]
-- *Main> :i []
-- type [] :: * -> *
-- data [] a = [] | a : [a]

-- In the following example, we’ll define the list type without using an infix constructor:



data List a = Nil | Cons a (List a)

-- *Main> nil = Nil
-- *Main> :t nil
-- nil :: List a

-- *Main> oneItem = (Cons "woohoo!" Nil)
-- *Main> :t oneItem
-- oneItem :: List [Char]

-- *Main> :kind List
-- List :: * -> *
-- *Main> :k []
-- [] :: * -> *


-- 11.17 Binary tree


data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)



insert' :: Ord a
    => a
    -> BinaryTree a
    -> BinaryTree a

insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

t1 = insert' 0 Leaf
t2 = insert' 3 t1
t3 = insert' 5 t2

-- *Main> t1 = insert' 0 Leaf
-- *Main> t1
-- Node Leaf 0 Leaf
-- *Main> t2 = insert' 3 t1
-- *Main> t2
-- Node Leaf 0 (Node Leaf 3 Leaf)
-- *Main> t3 = insert' 5 t2
-- *Main> t3
-- Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))


-- Write map for BinaryTree

-- definition of BinaryTree above, write a map function for the data structure.



mapTree :: (a -> b)
    -> BinaryTree a
    -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
--    Node undefined undefined undefined
      Node (mapTree f left) (f a) (mapTree f right)

t4 = mapTree (+1) t3

-- *Main> t3
-- Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))
-- *Main> t4
-- Node Leaf 1 (Node Leaf 4 (Node Leaf 6 Leaf))



testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"

-- *Main> mapOkay
-- "yup OK!"


-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
-- preorder = undefined
preorder Leaf = []
preorder (Node left a right) = [a] ++ ( preorder left )  ++ ( preorder right )

-- *Main> preorder t4
-- [1,4,6]


inorder :: BinaryTree a -> [a]
-- inorder = undefined
inorder Leaf = []
inorder (Node left a right) =  ( preorder left ) ++ [a] ++ ( preorder right )

postorder :: BinaryTree a -> [a]
-- postorder = undefined
postorder Leaf = []
postorder (Node left a right) =  ( preorder left ) ++ ( preorder right ) ++ [a] 

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

-- *Main> testPreorder
-- Preorder fine!


testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
-- *Main> testInorder 
-- Inorder fine!

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"
-- *Main> testPostorder 
-- Postorder fine!


-- Write foldr for BinaryTree. Given the definition of BinaryTree we have provided, write a catamorphism for binary trees:
-- any traversal order is fine
foldTree :: (b -> b -> b)
    -> b
    -> BinaryTree b
    -> b

foldTree f acc Leaf = acc                                         -- #1 - base case
foldTree f acc (Node Leaf a Leaf) = f a acc                       -- #2 - 2 leafs
foldTree f acc (Node Leaf a right) =  f a (foldTree f acc right)  -- #3 left is Leaf right is Tree
foldTree f acc (Node left a Leaf) = f a (foldTree f acc left)     -- #4 Left is tree and right is Leaf
foldTree f acc (Node left a right) = f a (f  (foldTree f acc left) ( foldTree f acc right))       -- #5 both sides are trees


-- foldTree f acc (Node left a right)  = f left ( foldree f acc right)



-- -- writing this out
-- -- *Main> t4
-- -- Node Leaf 1 (Node Leaf 4 (Node Leaf 6 Leaf))   -- 
-- --              <   ------ right  -----         >
-- -- f a (foldTree f acc right)  since Left is Leaf in our Tree to begin with

-- f 1       (foldTree f acc (Node Leaf 4 (Node Leaf 6 Leaf))
-- --                                     < --- right ---  >
-- f 1       ( f 4)   ( foldTree f acc  (Node Leaf 6 Leaf)  )
-- --                                         meets #2
-- f 1 (f 4  (f 6 acc)   )
-- if f is (+)  then acc is 0
-- f 1 ( f 4 6 )
-- f 1 10
-- 11

ft1 = insert' 8 Leaf
ft2 = insert' 3 ft1
ft3 = insert' 5 ft2
ft4 = insert' 25 ft3
ft5 = insert' 14 ft4

cft1 = insert' 'o' Leaf
cft2 = insert' 'c' cft1
cft3 = insert' 'e' cft2
cft4 = insert' 'y' cft3
cft5 = insert' 'r' cft4

charFunc :: Char -> Char -> Char
charFunc a b = if a > b then a else b


-- *Main> foldTree (+) 0 ft5
-- 55

-- *Main> foldTree (*) 1 ft5
-- 42000


-- *Main> ft5
-- Node (Node Leaf 3 (Node Leaf 5 Leaf)) 8 (Node (Node Leaf 14 Leaf) 25 Leaf)

-- -- writing this out

-- foldTree f  acc (Node Leaf 3 (Node Leaf 5 Leaf))        8             foldTree f acc (Node (Node Leaf 14 Leaf) 25 Leaf)

-- f 5 'a' return 'f'                                               f 14 'a' return 'o'

-- f 3 'f' = 'i'                                                    f 25 'o' return 'z'

-- f 8 'i'     = 'r'


-- 11.18 Chapter exercises


-- Ciphers
-- A Vigenère
-- cipher is another substitution cipher, based on a Caesar
-- cipher, but it uses a series of Caesar ciphers for polyalphabetic
-- substitution.

-- We’ll use the keyword “ALLY” here.
-- You repeat the keyword for as many characters as there are in
-- your original message:

-- MEET AT DAWN
-- ALLY AL LYAL
-- The number of rightward shifts to make to encode each character is set by the character of the keyword that lines up
-- with it. The “A” means a shift of 0, so the initial M will remain M. But the “L” for our second character sets a rightward shift
-- of 11, so “E” becomes “P.” And so on, therefore “meet at dawn”
-- encoded with the keyword “ALLY” becomes “MPPR AE OYWY.”

-- Prelude Data.Char> ord 'a'
-- 97
-- Prelude Data.Char> ord 'A'
-- 65
-- *Main Data.Char> ord 'L'
-- 76
-- *Main Data.Char> ord 'Y'
-- 89
-- Prelude Data.Char> ord 'Z'
-- 90
-- Prelude Data.Char> ord 'z'
-- 122

toBeCipheredString = "MEET AT DAWN"
toBeCipheredStringNoBlank = (filter (\x -> x /= ' ') toBeCipheredString)
-- *Main Data.Char> toBeCipheredStringNoBlank
-- "MEETATDAWN"

lengthCipher = length (filter (\x -> x /= ' ') toBeCipheredString)
-- *Main Data.Char> lengthCipher
-- 10

keyword = "ALLY"
lengthKeyword = length keyword
-- *Main Data.Char> lengthKeyword
-- 4


extendKeywordToCipher :: Int -> String -> Int -> String
extendKeywordToCipher lc k lk = if lc > lk then extendKey k ((div lc lk) + 1) else k     --- for now we just make the keyword much longer

finalKeyword = take lengthCipher (extendKeywordToCipher lengthCipher keyword lengthKeyword)
-- *Main Data.Char> finalKeyword
-- "ALLYALLYAL"

keywordOrd = map ord finalKeyword
-- *Main Data.Char> keywordOrd
-- [65,76,76,89,65,76,76,89,65,76]

-- shiftRight = map (\x -> x - 65) keywordOrd

shiftRight = map (\x -> x - 65) (map ord finalKeyword)
-- *Main Data.Char> shiftRight
-- [0,11,11,24,0,11,11,24,0,11]


extendKey :: String -> Int -> String
extendKey k 0  = []
extendKey k tm = k ++ extendKey k (tm-1)

makeCipherTuples :: [Int] -> String -> [(Char,Int)]
makeCipherTuples [k] [] = []
makeCipherTuples [k] [x] = [(x,k)]
makeCipherTuples (k:ks) [] = []
makeCipherTuples (k:ks) (x:xs) = (x,k) : makeCipherTuples ks xs

cipherTuples = makeCipherTuples shiftRight toBeCipheredStringNoBlank
-- *Main Data.Char> cipherTuples
-- [('M',0),('E',11),('E',11),('T',24),('A',0),('T',11),('D',11),('A',24),('W',0),('N',11)]

shiftedCipher :: [(Char, Int)] -> String
shiftedCipher [] = []
shiftedCipher [(c,i)] = if ((ord c) + i) > 90 then [chr ((mod ((ord c) + i) 90 ) + 64)] else [chr ((ord c) + i)]
shiftedCipher ((c,i): xs) = (if ((ord c) + i) > 90 then chr ((mod ((ord c) + i) 90 ) + 64) else chr ((ord c) + i) ) : shiftedCipher xs

vigenèreNoBlank = shiftedCipher cipherTuples
-- *Main Data.Char> vigenèreNoBlank
-- "MPPRAEOYWY"


-- we removed spaces from string before now we put it back
cipherWSpaces :: String -> String -> String 
cipherWSpaces [] _ = []
cipherWSpaces [x] [] = []
cipherWSpaces [x] [v] = if x /= ' ' 
                              then [v] 
                              else []
cipherWSpaces (x:xs) (v:vs) = if x /= ' ' 
                              then v : cipherWSpaces xs vs           -- x and v actually would line up
                              else  cipherWSpaces xs (v:vs)          -- here we drop the x which blank and not consume the v    

finalVigenere = cipherWSpaces toBeCipheredString vigenèreNoBlank 
-- *Main Data.Char> finalVigenere
-- "MPPRAEOYWY"

-- As-patterns


asIsF :: Show a => (a, b) -> IO (a, b)
asIsF t@(a, _) = do
    print a
    return t

-- Here, we pattern match on a tuple so we can get at the first value for printing, but use the @ symbol to introduce a binding
-- named t in order to refer to the whole tuple rather than just a part:

-- *Main Data.Char> asIsF (1,2)
-- 1
-- (1,2)
-- *Main 

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- *Main Data.Char> doubleUp []
-- []
-- *Main Data.Char> doubleUp [1]
-- [1,1]
-- *Main Data.Char> doubleUp [1, 2]
-- [1,1,2]
-- *Main Data.Char> doubleUp [1, 2, 3]
-- [1,1,2,3]


doubleUp' :: [a] -> [a]
doubleUp' [] = []
doubleUp' t@(_:xs) = t ++ xs 


suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []
-- *Main Data.Char> suffixes "hello"
-- ["hello","ello","llo","lo","o"]

-- xs' would be bound to the string "ello".

-- xs would be bound to the string "hello".

-- The @ pattern lets you give a name to a variable while also matching its structure and possibly giving name to the components.




-- Use as-patterns to implement the following functions:


-- 1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous:

isSubseqOf :: (Eq a)
    => [a]
    -> [a]
    -> Bool

isSubseqOf [] [] = False
isSubseqOf [a] [b] = if a == b 
                     then True 
                     else False                                 -- False because we have exhausted 2nd list
isSubseqOf [a] [] = False                                       -- we came to end of 2nd list and still this value from 1st list is not found.
isSubseqOf [a] (b:bs) = if a==b 
                        then True 
                        else isSubseqOf [a] bs                  -- we are last letter of first list
isSubseqOf (a:as) [b] = False                                   -- Since we exhausted the 2nd list so cant match rest of "as" anyways even if a==b

isSubseqOf (a:as) (b:bs) = if a==b 
                           then True && isSubseqOf as bs        -- move on to rest of 1st list
                           else isSubseqOf (a:as) bs            -- not found so move on to next letter 

-- Matches expected to actual results
-- *Main Data.Char> isSubseqOf "blah" "blahwoot"
-- True
-- *Main Data.Char> isSubseqOf "blah" "wootblah"
-- True
-- *Main Data.Char> isSubseqOf "blah" "wboloath"
-- True
-- *Main Data.Char> isSubseqOf "blah" "wootbla"
-- False
-- *Main Data.Char> isSubseqOf "blah" "halbwoot"
-- False
-- *Main Data.Char> isSubseqOf "blah" "blawhoot"
-- True

-- this will work ONLY if sequence is maintained 
-- *Main Data.Char> isSubseqOf "blah" "lbawhoot"          -- here are all letters are there but order is not
-- False



-- Now using the AS-PATTERNS -- 
isSubseqOf' :: (Eq a)
    => [a]
    -> [a]
    -> Bool

isSubseqOf' [x] [] = False
isSubseqOf' [x] [y] = if x == y
                      then True
                      else False
isSubseqOf' [x] t@(y:ys) = if elem x t 
                           then True 
                           else False 
isSubseqOf' (x:xs) t@(y:ys) = if elem x t 
                              then True && (isSubseqOf' xs (restString (elemIndex x t) t ))
                              else False 



p = elemIndex 'b' "halbwoot"
restString :: Maybe Int -> [a] -> [a]
restString (Just a) s = drop (a +1) s
restString Nothing s = []


-- *Main Data.Char Data.List> isSubseqOf' "blah" "blahwoot"
-- True
-- *Main Data.Char Data.List> isSubseqOf' "blah" "wootblah"
-- True
-- *Main Data.Char Data.List> isSubseqOf' "blah" "wboloath"
-- True
-- *Main Data.Char Data.List> isSubseqOf "blah" "wootbla"
-- False
-- *Main Data.Char Data.List> isSubseqOf "blah" "halbwoot"
-- False
-- *Main Data.Char Data.List> isSubseqOf "blah" "blawhoot"
-- True
-- *Main Data.Char Data.List> isSubseqOf "blah" "lbawhoot"
-- False


-- 2. Split a sentence into words, then tuple each one with its capitalized form:

capitalizeWords :: String
    -> [(String, String)]

capitalizeWords str = map (\(x,(y:ys)) -> (x, (( chr ((ord y) - 32) ): ys) ) ) (map (\x -> (x,x)) (words str))

-- Expected results:
-- Prelude> capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]

-- Actual 
-- *Main Data.Char Data.List> capitalizeWords "hello world"
-- [("hello","Hello"),("world","World")]

initString = "hello world"

wordList = words initString

tupleList = map (\x -> (x,x)) wordList

tupleListCaps = map (\(x,(y:ys)) -> (x, (( chr ((ord y) - 32) ): ys) ) ) tupleList   

-- *Main Data.Char Data.List> tupleListCaps
-- [("hello","Hello"),("world","World")]











