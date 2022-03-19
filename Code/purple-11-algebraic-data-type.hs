

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


-- 4. What is the type of Husky 10?    -- type is Doggies
-- *Main> :t Husky 10
-- Husky 10 :: Num a => Doggies a


-- 5. What is the type of Husky (10 :: Integer)?     -- should be Integer a => Doggies Integer
-- *Main> :t Husky (10 :: Integer)
-- Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?   -- since "Scooby Doo" resolves to String. It will be Doggies String or [Char]
-- *Main> :t Mastiff "Scooby Doo"
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor?

-- 8. What is the type of DogueDeBordeaux?

-- 9. What is the type of DogueDeBordeaux "doggie!"