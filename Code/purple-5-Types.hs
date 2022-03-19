

-- 5.6 Type inference

-- typeInference1.hs
-- typeInference2.hs



-- Exercises: Apply yourself


-- 1

-- *TypeInference2> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- *TypeInference2> myConcat x = x ++ " yo"
-- *TypeInference2> :t myConcat
-- myConcat :: [Char] -> [Char]

-- 2

-- *TypeInference2> :t (*)
-- (*) :: Num a => a -> a -> a
-- *TypeInference2> myMult x = (x / 3) * 5
-- *TypeInference2> :t myMult
-- myMult :: Fractional a => a -> a


-- 3 

-- *TypeInference2> :t take
-- take :: Int -> [a] -> [a]
-- *TypeInference2> myTake x = take x "hey you"
-- *TypeInference2> :t myTake
-- myTake :: Int -> [Char]

-- *TypeInference2> myTake3 x y = take x "aefakejfsdf"
-- *TypeInference2> :t myTake3
-- myTake3 :: Int -> p -> [Char]


-- *TypeInference2> myTake1 = take 5 "hey Yuo there"
-- *TypeInference2> :t myTake1
-- myTake1 :: [Char]



--4
-- *TypeInference2> myCom x = x > (length [1..10])
-- *TypeInference2> :t myCom
-- myCom :: Int -> Bool


-- 5
-- *TypeInference2> myAlph x = x < 'z'
-- *TypeInference2> :t myAlph
-- myAlph :: Char -> Bool





-- 5.7 Asserting types for declarations


-- 5.8 Chapter exercises
-- DetermineTheType.hs for exercises





-- Given a type, write the function


myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))

-- at some point i can define:
xToY' :: Int -> Char
xToY' i 
      | i==1 = 'a'
      | i==2 ='b'
      | i==3 = 'c'
      | True = 'z'

yToZ' :: Char -> Bool
yToZ' c = if c > 'Q' then True else False
myTuple :: (String, Int)
myTuple = ("Rafa", 10)

-- *Main> myFunc xToY' yToZ' 4  myTuple
-- ("Rafa",True)

-- myFunc (x -> y) (y->z) _ (a, x) = (a, (yToZ (xToY x)))

-- 1. There is only one function definition that type checks
-- and doesn’t go into an infinite loop when you run it:
i :: a -> a
-- i = undefined

i  = (\x -> x)

i' :: a -> a
i' a = a


funcAdd :: Int -> Int -> Int 
funcAdd x y = x + y

-- 2. There is only one version that works:
-- c :: a -> b -> a
-- c = undefined

c2 :: a -> b -> a

c2 a b = (\b -> a) ( (\a -> b) a )









