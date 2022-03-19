import Data.List

data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True


data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun


-- day of week and numerical day of month
data Date =
    Date DayOfWeek Int
 
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

-- *Main> (==) Mon Tue
-- False

instance Eq Date where
    (==) (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') =
         weekday == weekday'
      && dayOfMonth == dayOfMonth'


-- Prelude> Date Thu 10 == Date Thu 10
-- True
-- Prelude> Date Thu 10 == Date Thu 11
-- False
-- Prelude> Date Thu 10 == Date Weds 10
-- False


-- Exercises: Eq Instances

-- data TisAnInteger = 
--     TisAn Integer

-- instance Eq TisAnInteger where
--     (==) TisAn = TisAn


-- data Identity'' a =
--     Identity'' a

-- instance Eq Identity'' a where
--     (==)  Identity'' v = v

data DayOfWeek' =
    Monday | Tuessday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show


data Date' = Date' DayOfWeek' Int deriving Show





check' :: Ord a => a -> a -> Bool
check' a a' = a == a'
-- *Main> check' 5 6
-- False
-- *Main> check' 5 5
-- True

check'' :: Eq a => a -> a -> Bool
check'' a a' = a == a'
-- *Main> check'' 5 5
-- True
-- *Main> check'' 5 7
-- False


-- Enum

-- *Main> enumFromThenTo 0 10 100
-- [0,10,20,30,40,50,60,70,80,90,100]
-- *Main> enumFromThenTo 'a' 'c' 'z'
-- "acegikmoqsuwy"




-- 6.10 Show
data Mood = Blah
instance Show Mood where
    show _ = "Blah ha ha ha "
-- *Main> Blah
-- Blah ha ha ha 


data Mood' = Blah' deriving Show
-- *Main> Blah'
-- Blah'



class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

-- pretend newtype is data for now
newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n


newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n


sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA = toNumber a
          integerOfAPrime = toNumber a'
          summed = integerOfA + integerOfAPrime


-- *Main> sumNumberish (Age 10) (Age 10)
-- Age 20
-- *Main> sumNumberish (Year 1970) (Year 2022)
-- Year 3992


-- my own experiments - 

-- i tried to define functions here itself but was not possible since 
-- class MyNumberish a where
--     myfromNumber :: Integer -> a
--     mytoNumber :: a -> Integer
--     -- myfromNumber n = a n 
--     -- mytoNumber (Age n) = n
--     -- -- MyfromNumber n = Year n
--     -- mytoNumber (Year n) = n

-- -- pretend newtype is data for now
-- newtype MyAge =
--     MyAge Integer
--     deriving (Eq, Show)


-- newtype MyYear =
--     MyYear Integer
--     deriving (Eq, Show)




-- sumNumberish :: Numberish a => a -> a -> a
-- sumNumberish a a' = fromNumber summed
--     where integerOfA = toNumber a
--           integerOfAPrime = toNumber a'
--           summed = integerOfA + integerOfAPrime




-- Concrete types imply all the type classes they provide
add :: Int -> Int -> Int
add x y = x + y

-- 6.14 Chapter exercises


--Does it type check?
x :: Int -> Int
x blah = blah + 20
-- printIt :: IO ()
-- printIt = putStrLn (show x)

-- added deriving Show
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- *Main> x = Person True
-- *Main> x
-- Person True


-- Eq was not derived to do == , so added Eq
-- we dont need to declare what type x is ... even if you declare you can always give some other type in Application.
data Mood''' = Blah'''
    | Woot''' 
    deriving (Eq, Show, Ord)
settleDown x = if x == Woot'''
               then Blah'''
               else x

--3b - you cannot do settleDown 9 coz there is no way to compare 9 to Woot
-- Blah > Woot - cant do since Ord is not derived. now adding Ord

--4.
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool"                  -- i guess even data you can partially declare and later supply the other argument. 
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?
data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)


-- *Main> phew = Papu "chases" True

-- <interactive>:211:13: error:
--     • Couldn't match expected type ‘Rocks’ with actual type ‘[Char]’
--     • In the first argument of ‘Papu’, namely ‘"chases"’
--       In the expression: Papu "chases" True
--       In an equation for ‘phew’: phew = Papu "chases" True

-- <interactive>:211:22: error:
--     • Couldn't match expected type ‘Yeah’ with actual type ‘Bool’
--     • In the second argument of ‘Papu’, namely ‘True’
--       In the expression: Papu "chases" True
--       In an equation for ‘phew’: phew = Papu "chases" True
-- *Main> phew = Papu (Rocks "chases") (Yeah True)

-- *Main> truth = Papu (Rocks "chomskydoz") (Yeah True)
-- *Main> truth
-- Papu (Rocks "chomskydoz") (Yeah True)


equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- *Main> Papu (Rocks "chomskydoz") (Yeah True) == Papu (Rocks "chomskydoz") (Yeah True)
-- True
-- *Main> Papu (Rocks "chomskydoz") (Yeah True) == Papu (Rocks "Rafa") (Yeah True)
-- False


-- Match the types
i :: Num a => a
--i :: a          -- this will fail as 1 is a num
i = 1
-- purple-6-TypeClasses.hs:267:5: error:
--     • No instance for (Num a) arising from the literal ‘1’
--       Possible fix:
--         add (Num a) to the context of
--           the type signature for:
--             i :: forall a. a
--     • In the expression: 1
--       In an equation for ‘i’: i = 1
--     |
-- 267 | i = 1
--     |     ^
-- Failed, no modules loaded.


-- Prelude> :t 1
-- 1 :: Num p => p

-- f :: Float
-- f :: Num a => a   -- Will error when you do f = 1.0
f :: Fractional a => a
f = 1.0

f' :: Fractional a => a -> a -> a
f' a b = a + b

-- *Main> f' 1 2
-- 3.0




-- f'' :: Float
f'' :: RealFrac a => a
f'' = 1.0


-- freud :: a -> a
-- freud :: Ord a => a -> a    -- is fine since we are not dooing anything with x anyway
freud :: Int -> Int   -- is also fine
freud x = x


-- 7 
myX = 1 :: Int
sigmund :: Int -> Int      -- is fine
-- sigmund :: a -> a      -- will give error because you are forcing a to Int in the definition --  • Couldn't match expected type ‘a’ with actual type ‘Int’
-- sigmund :: Num a => a -> a     -- this should error since you are forcing Num to Int
sigmund x = myX                 -- here myX concretizes only to Int - anything else would error  


-- 9 
--jung :: Ord a => [a] -> a
jung :: [Int] -> Int             -- should be fine but can only do on Int's
jung xs = head (sort xs)

-- 10

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)
-- *Main> :i sort
-- sort :: Ord a => [a] -> [a]

-- 11
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a       -- this will NOT work since mySort is only Char and wont work with other types.
signifier xs = head (mySort xs)

-- chk :: Eq b => (a -> b) -> a -> b -> Bool
-- chk f a b = 









