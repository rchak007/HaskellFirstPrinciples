module Addition where
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

sayHello :: IO ()
sayHello = putStrLn "hello!"


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise =
               go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy 0 b = 0
multiplyBy a b = b + multiplyBy (a-1) b




-- *Addition> :t hspec
-- hspec :: Spec -> IO ()

-- *Addition> :t describe
-- describe :: String -> SpecWith a -> SpecWith a

-- *Addition> :t shouldBe
-- shouldBe :: (Show a, Eq a) => a -> a -> Expectation

-- *Addition> :t property
-- property :: Testable prop => prop -> Property

-- *Addition> main

-- Addition
--   1 + 1 is greater than 1 [✔]

-- Finished in 0.0028 seconds
-- 1 example, 0 failures

-- after adding 2+2
-- *Addition> main

-- Addition
--   1 + 1 is greater than 1 [✔]
--   2 + 2 is equal to 4 [✔]

-- Finished in 0.0008 seconds
-- 2 examples, 0 failures


-- After adding dividedBy
-- *Addition> main

-- Addition
--   1 + 1 is greater than 1 [✔]
--   2 + 2 is equal to 4 [✔]
--   15 divided by 3 is 5 [✔]
--   22 divided by 5 is 4 remainder 2 [✔]

-- Finished in 0.0025 seconds
-- 4 examples, 0 failures


-- *Addition> main

-- Addition
--   1 + 1 is greater than 1 [✔]
--   2 + 2 is equal to 4 [✔]
--   x + 1 is always greater than x [✔]
--     +++ OK, passed 100 tests.
-- Divide
--   15 divided by 3 is 5 [✔]
--   22 divided by 5 is 4 remainder 2 [✔]
-- MultiplyBy
--   10 mult 100 is 1000 [✔]

-- Finished in 0.0048 seconds
-- 6 examples, 0 failures





-- sample 

-- *Addition> sample (arbitrary :: Gen Char)
-- '\1050861'
-- '\ETX'
-- 'f'
-- '#'
-- ']'
-- '\b'
-- 'H'
-- '\194808'
-- 'x'
-- '\DC4'
-- '\t'

-- *Addition> sample (arbitrary :: Gen Int)
-- 0
-- 0
-- -1
-- -3
-- -5
-- 2
-- -11
-- 4
-- -10
-- 6
-- 0


-- *Addition> sample (arbitrary :: Gen [Char])
-- ""
-- "I"
-- "\GS}"
-- "p~"
-- "\6438\12615i"
-- "\62460gh"
-- "?I"
-- ""
-- "(a"
-- "6!un"
-- "\CANJK\157334\50289d<A\\{N\SYN\1031214l'n\"Y\175630"

-- *Addition> sample (arbitrary :: Gen Double)
-- 0.0
-- -1.0884097367790522
-- -1.5890006041070843
-- -4.184374090338195
-- 1.0399690493755174e-2
-- 9.164335120753627
-- 5.413494113743758
-- -7.317473269500109
-- -15.210710561444944
-- -5.733096457527527
-- -15.252130325528507


-- *Addition> sample' (arbitrary :: Gen Double)
-- [0.0,-0.8094072821626052,-0.47555358116863866,0.595449385075849,-5.3992635342552955,-7.282973099963234,-9.635412903748701,8.676054067550647,14.357865194083567,11.25962813469339,14.775122674956913]
-- *Addition> sample' (arbitrary :: Gen Int)
-- [0,-2,-1,0,4,-10,-6,6,-11,0,-20]
-- *Addition> sample' (arbitrary :: Gen Char)
-- "\1106370\v3\169702MJ\13893S[9\v"

-- *Addition>  sample' (arbitrary :: Gen [Char])
-- ["","","CY\DC4e","vD\DC3\SYNx\DC3","B\15238(\60797WBj\138179","D\1107743\1107714W\RS\a","\1012610\GS","'y\1041369e2$\"\1062700\1066405S","Q\37462\DLE>2h_\ETX\1096217\&9\RS\132671B\b","`<U>\1043696\143309y\EMPh","\ACK\42795\180271\1028644$"]


-- Generating values different ways 
-- #1 return
-- trivial generator of values
trivialInt :: Gen Int
trivialInt = return 1

-- *Addition> sample' trivialInt
-- [1,1,1,1,1,1,1,1,1,1,1]

-- #2 elements
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

-- *Addition> sample' oneThroughThree
-- [3,2,3,2,1,2,2,3,2,2,3]

-- #3 Tweak the elements increase probabilty of 2
oneThroughThree' :: Gen Int
oneThroughThree' =
    elements [1, 2, 2, 2, 2, 3]     
-- *Addition> :sample' oneThroughThree'
-- [3,2,3,3,2,2,2,1,2,3,2]

-- *Addition> :t sample
-- sample :: Show a => Gen a -> IO ()
-- *Addition> :t sample'
-- sample' :: Gen a -> IO [a]

-- #4 - choose 
genBool :: Gen Bool
genBool = choose (False, True)
-- *Addition> sample' genBool
-- [False,True,True,True,True,False,True,False,True,False,True]

-- same elements we used already 
genBool' :: Gen Bool
genBool' = elements [False, True]

-- *Addition> sample' genBool'
-- [True,False,True,False,False,True,False,False,False,True,True]

-- elements same thing but with Ordering
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
-- *Addition> sample' genOrdering 
-- [GT,LT,LT,GT,LT,EQ,EQ,EQ,EQ,LT,GT]

-- #5 elements with Enumeration 
genChar :: Gen Char
genChar = elements ['a'..'z']

-- *Addition> sample' genChar
-- "ynaoukcflkv"

-- #6 - now with type not being concrete!! and also multiple arguments
genTuple :: (Arbitrary a, Arbitrary b)
    => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

--  *Addition> sample genTuple
--  ((),())
-- ((),())
-- ((),())
-- ((),())
-- ((),())

-- *Addition> type G = Gen (Int, Float)
-- *Addition> sample (genTuple :: G)
-- (0,0.0)
-- (-1,-1.5097861)
-- (4,1.0471277)
-- (4,-3.6592126)
-- (8,4.5863256)
-- (-8,-4.3486323)
-- (4,10.778284)
-- (13,6.7398257)
-- (-7,1.9592212)
-- (10,9.051466)
-- (8,6.0164833)


-- *Addition> type G = Gen ([()], Char)
-- *Addition> sample (genTuple :: G)
-- ([],'c')
-- ([],')')
-- ([(),(),()],'\SI')
-- ([(),(),(),(),()],'A')
-- ([],'e')
-- ([()],'\145280')
-- ([(),(),()],'B')
-- ([(),(),(),(),(),(),(),()],'3')
-- ([(),(),(),(),(),(),(),(),(),(),(),(),()],'\1106337')
-- ([(),(),(),(),(),(),()],'$')
-- ([(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()],'\STX')

test001 :: Gen Int 
test001 = elements [100..200]
-- *Addition> sample' test001
-- [172,169,110,109,107,144,114,197,118,125,146]

test002 :: (Arbitrary a) 
    => Gen a
test002 = do
    a <- arbitrary 
    return a
-- *Addition> type G = Gen Int
-- *Addition> sample' (test002 :: G)
-- [0,2,0,1,-4,7,2,10,9,-11,14]


-- my own test.. you still have to give Arbitrary -cant get away with that to come with some Gen
genTupleTest1 :: (Num a, Num b, Arbitrary a, Arbitrary b)
    => Gen (a,b)
genTupleTest1 = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)
-- *Addition> type G = Gen (Int, Int)
-- *Addition> sample' (genTupleTest1 :: G)
-- [(0,0),(-2,-1),(3,-1),(-2,1),(7,-6),(-4,-5),(6,-12),(-9,14),(11,0),(5,13),(14,0)]
-- *Addition> sample' genTupleTest1
-- --- gives some warnings too
-- [(0,0),(-1,-1),(-3,-2),(-5,2),(7,2),(-7,7),(-12,-12),(12,-8),(1,11),(-9,-5),(5,-14)]


genThreeple :: (Arbitrary a, Arbitrary b,
        Arbitrary c)
    => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)




-- *Addition> sample (genThreeple :: Gen (Int, Char, Float))
-- (0,'\94238',0.0)
-- (-1,'\46005',0.26824743)
-- (4,'Y',1.2385244)
-- (-4,'\1084559',-5.1590405)
-- (6,'\EOT',-2.339334)
-- (9,'\136596',0.6390913)
-- (-7,'`',8.748736)
-- (2,'Y',-10.1295805)
-- (-2,'\v',-6.465278)
-- (2,'3',-11.886599)
-- (10,'Y',-6.23506)

genEither :: (Arbitrary a, Arbitrary b)
    => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]


genInt100 :: Gen Int 
genInt100 = elements [1..100]

-- genEither' :: Gen (Either Int Int)
-- genEither' = do 
--     a <- elements [1..100]
--     elements [Left a, Right a]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]
-- frequency :: [(Int, Gen a)] -> Gen a

-- *Addition> type G = Gen (Maybe Int)
-- *Addition> sample (genMaybe :: G)
-- Just 0
-- Nothing
-- Nothing
-- Just (-3)
-- Nothing
-- Just 5
-- Just (-12)
-- Nothing
-- Nothing
-- Just (-13)
-- Nothing

-- *Addition> type G = Gen (Maybe Int)
-- *Addition> sample' (genMaybe' :: G)
-- [Just 0,Nothing,Just 0,Just 3,Nothing,Just (-5),Just 0,Just 11,Nothing,Nothing,Nothing]

-- *Addition> type E = Gen (Either Int Int)
-- *Addition> sample' (genEither :: E)
-- [Right 0,Left 0,Left 0,Right 2,Left 7,Right 2,Right (-4),Right (-1),Left 14,Left 4,Left (-7)]

-- *Addition> type IC = Gen (Either Int Char)

-- *Addition> sample' (genEither :: IC)
-- [Right 'w',Left (-2),Left 2,Right ',',Left 7,Right 'e',Right '\97235',Right '\v',Left 11,Left 11,Right 'h']



-- Using QuickCheck without Hspecx

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreater' :: Int -> Bool
prop_additionGreater' x = x + 0 > x                --- Assert something untrue

runQc :: IO ()
runQc = do 
    quickCheck prop_additionGreater
    quickCheck prop_additionGreater'               --- Assert something untrue
    quickCheck propRandomFunc

-- *Addition> runQc
-- +++ OK, passed 100 tests.

-- *Addition> runQc
-- +++ OK, passed 100 tests.
-- *** Failed! Falsified (after 1 test):             --- Assert something untrue
-- 0









-- 14.5 Morse code

-- Did separate folder here - 

-- /Users/chakravartiraghavan/Documents/Typora1/Blockchain/CardanoTraining/Haskell/Projects/morse


-- 14.6 Arbitrary instances


data Trivial =
    Trivial
    deriving (Eq, Show) 

trivialGen :: Gen Trivial
trivialGen =
    return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen
 
-- *Addition> :sample' trivialGen    -- this actually printed when instance is commented out too 
-- [Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial]

data Identity a =
    Identity a
    deriving (Eq, Show)

identityGen :: Arbitrary a =>
    Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a =>
        Arbitrary (Identity a) where
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- *Addition> sample' identityGenInt    -- this still prints when i comment out the instance Aribitrary
-- [Identity 0,Identity (-2),Identity 0,Identity 3,Identity 0,Identity 3,Identity (-3),Identity 1,Identity (-11),Identity (-16),Identity 10]


-- Arbitrary products
data Pair a b =
    Pair a b
    deriving (Eq, Show)
pairGen :: (Arbitrary a,
    Arbitrary b) =>
    Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)


instance (Arbitrary a,
    Arbitrary b) =>
    Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- *Addition> sample' pairGenIntString    -- -again prints even w/o the instance too since 
--                                               i guess the pairGen already is defining some arbitrary stuff
-- [Pair 0 "",Pair 1 "",Pair (-2) "\NAK",Pair 0 "",Pair 1 "v\DC1Z\RS(7jN",Pair 5 "\148966",Pair 11 "A\61018\1025613j\1090141k\1055721",Pair 10 "\SOHGD{fxL\154943",Pair (-12) "\ENQ\DLE\DC2]\1034218]\1937b\ESC\1086337FZ\3112H\EOTq",Pair 17 "RnX0C\139661\EM\49656q>\\\1092687\r",Pair (-1) "H\DC4%`\198834f"]



data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)


-- import Test.QuickCheck.Gen (oneof)

-- equal odds for each
sumGenEqual :: (Arbitrary a,
    Arbitrary b) =>
    Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a,
           return $ Second b]

-- oneof

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- *Addition> sample' sumGenCharInt 
-- [First '*',Second 1,First '.',Second 4,First '}',First '\NUL',Second (-2),Second 0,First 'O',First '\1107049',Second 15]


-- we can choose different weighting probabilities than equal distribution.

sumGenFirstPls :: (Arbitrary a,
    Arbitrary b) =>
    Gen (Sum a b)

sumGenFirstPls = do
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return $ First a),
              (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-- *Addition> sample' sumGenCharIntFirst   -- we did not create an Aribtrary instance actually for Sum
-- [First '\154180',First 'C',First '\1045143',First 'B',First '\\',First '\f',First '4',First 'Y',Second 16,First '9',First '\1069179']

propRandomFunc :: Int -> Bool 
propRandomFunc i = (i + 7) > (i + 5)
-- *Addition> quickCheck propRandomFunc 
-- +++ OK, passed 100 tests.

-- 14.7 Chapter exercises







main :: IO ()
main = hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do
        (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do
        2 + 2 `shouldBe` 4
      it "x + 1 is always\
        \ greater than x" $ do
       property $ \x -> x + 1 > (x :: Int)
    describe "Divide" $ do
      it "15 divided by 3 is 5" $ do
        dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is\
            \ 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    describe "MultiplyBy" $ do
      it "10 mult 100 is 1000" $ do
        multiplyBy 10 100 `shouldBe` 1000


-- *Addition> main

-- Addition
--   1 + 1 is greater than 1 [✔]
--   2 + 2 is equal to 4 [✔]
--   x + 1 is always greater than x [✔]
--     +++ OK, passed 100 tests.
-- Divide
--   15 divided by 3 is 5 [✔]
--   22 divided by 5 is 4 remainder 2 [✔]
-- MultiplyBy
--   10 mult 100 is 1000 [✔]

-- Finished in 0.0407 seconds
-- 6 examples, 0 failures


