
import Data.Time

xs = map show [1..5]
y = foldr (\x y -> concat  ["(",x,"+",y,")"]) "0" xs

-- *Main> xs
-- ["1","2","3","4","5"]


-- ["(",x,"+",y,")"] "5" 0"

-- ["(","5","+","0",")"]



-- [] -> z
-- (x:xs) -> f x (foldr f z xs)

-- \x y -> concat  ["(",x,"+",y,")"] "1"      ( foldr f "0" ["2", "3", "4", "6" ]

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs =
    foldr (\x b -> f x || b) False xs

-- The following should work despite being an infinite list:
-- Prelude> myAny even [1..]         -- because the 2nd element gives us True and then due to || (or) it need to evaluate remaining since results will remain True
-- True


-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

--  foldr const 0 [1..5]
--  const 1 ( foldr const 0 [2..5])
-- but since const takes only 1st arg it need not traverse remaining. 
-- const 1 _
-- 1

-- *Main> foldr const 0 [1, undefined, undefined]       -- thats why here due to const it does not traverse remaining elements since its not needed.
-- 1



-- ***************************************    two stages, traversal and folding     ****************************************
-- One initially non-obvious aspect of folding is that it happens in two stages, traversal and folding. Traversal is the stage
-- in which the fold recurses over the spine. Folding refers to the evaluation or reduction of the folding function applied
-- to the values. All folds recurse over the spine in the same direction; the difference between left folds and right folds is
-- in the association, or parenthesization, of the folding function

--  foldr (/) 2 [1..5]
-- ***************************************    1st stages, traversal
--  (/) 1 ( foldr (/) 2 [2..5])
-- (/) 1 ( (/) 2 (foldr (/) 2 [3..5]))
-- (/) 1 ( (/) 2 ((/) 3 (foldr (/) 2 [4..5]  ))
-- (/) 1 ( (/) 2 ((/) 3 (  (/) 4 (foldr (/) 2 [5])  ))
-- (/) 1 ( (/) 2 ((/) 3 (  (/) 4 (  (/) 5 (foldr (/) 2 [] ) )  ))

-- ***************************************    2nd stage, folding 

-- (/) 1 ( (/) 2 ((/) 3 (  (/) 4 (  (/) 5 (2) )  ))
-- (/) 1 ( (/) 2 ((/) 3 (  (/) 4 (  2.5 )  ))

-- (/) 1 ( (/) 2 ((/) 3 (  1.6  ))
-- (/) 1 ( (/) 2 ( 1.875 )
-- (/) 1 ( 1.0666666666666667)
-- 0.9375


-- *Main> foldr (/) 2 [1..5]
-- 0.9375

-- map 
-- *Main> map (+2) [1,3,4,undefined]
-- [3,5,6,*** Exception: Prelude.undefined
-- Crucially, map doesn’t traverse the whole list and apply the function immediately. The function
-- is applied to the values you force out of the list one by one.



-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

conc = concat
f x y = conc ["(",x,"+",y,")"]
x1 = foldl f "0" (map show [1..5])
-- "(((((0+1)+2)+3)+4)+5)"

-- foldl conc ["(",x,"+",y,")"] ( conc ["(",x,"+",y,")"] "0" "1" ) ["2","3","4","5"]
-- foldl conc ["(",x,"+",y,")"] "(0+1)" ["2","3","4","5"]
-- foldl conc ["(",x,"+",y,")"] (conc ["(",x,"+",y,")"] "(0+1)" "2" ) ["3","4","5"]
-- foldl conc ["(",x,"+",y,")"] ("((0+1)+2)") [3","4","5"]

-- foldl conc ["(",x,"+",y,")"]    (conc ["(",x,"+",y,")"] "((0+1)+2)" "3")       [4","5"]

-- foldl conc ["(",x,"+",y,")"]    ("(((0+1)+2)+3)")       [4","5"]
-- foldl conc ["(",x,"+",y,")"]    (  conc ["(",x,"+",y,")"]  "(((0+1)+2)+3)") "4" )         [5"]

-- foldl conc ["(",x,"+",y,")"]    (  "((((0+1)+2)+3)+4)" )         [5"]

-- foldl conc ["(",x,"+",y,")"]    (  conc ["(",x,"+",y,")"]  "((((0+1)+2)+3)+4)"  "5")         []

-- foldl conc ["(",x,"+",y,")"]    (  "(((((0+1)+2)+3)+4)+5)")         []        -- same as foldl acc [] = acc

-- "(((((0+1)+2)+3)+4)+5)"
-- *Main> x1
-- "(((((0+1)+2)+3)+4)+5)"







-- foldr (^) 2 [1..3]
-- -- foldr f z [] = z
-- -- foldr f z (x:xs) = f x (foldr f z xs)
-- ((^) 1) (foldr (^) 2 [2..3])


-- ((^) 1) ((^) 2) (foldr (^) 2 [3])

-- ((^) 1) ((^) 2) ((^) 3) (foldr (^) 2 [])

-- ((^) 1) ((^) 2) ((^) 3) (2)

-- ((^) 1) ((^) 2) (9)
-- ((^) 1) (512)
-- 1



-- foldl (^) 2 [1..3]
-- -- foldl f acc [] = acc
-- -- foldl f acc (x:xs) = foldl f (f acc x) xs
-- foldl (^)   ( (^) 2 1 )    [2..3]
-- --           (f acc  x )    (xs)

-- --foldl f         < acc >        (x:xs)

-- foldl (^)   ( (^)  ( (^) 2 1 )     2  )          [3] 
-- --     f     <           acc         >          (x:xs)

-- foldl (^)   ( (^)  ( (^)  ( (^) 2 1 )     2  )  3 )          [] 
-- --     f    <           acc                       >          (x:xs)          -- Fits the base case

-- acc
--  ( (^)  ( (^)  ( (^) 2 1 )     2  )  3 )
--                 < evals  >

-- ( (^)  ( (^)  ( 2 )     2  )  3 )
--        <      evals        >

-- ( (^)  ( 4  )  3 )

-- 64

-- *Main> foldl (^) 2 [1..3]
-- 64





-- foldr (:) [] [1..3]

-- (:) 1     (  foldr (:) [] [2..3])

-- (:) 1     (  (:) 2    ( foldr (:) [] [3] )   )

-- (:) 1     (  (:) 2    ( (:) 3   ( foldr (:) [] [] ) )   )

-- (:) 1     (  (:) 2    ( (:) 3   ( [] ) )   )

-- (:) 1     (  (:) 2    ( [3] ) )  

-- (:) 1  [2,3]

-- [1,2,3]


-- You cannot do 
-- foldl (:) [] [1..3]

-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldl (:)   (  (:) [] 1  )        --- because (:) [] 1 will fail  --- 

-- *Main> foldl (flip (:)) [] [1..3]
-- [3,2,1]
-- *Main> reverse ( foldl (flip (:)) [] [1..3])
-- [1,2,3]



--  foldr const 0 [1..5]
-- -- -- foldr f z [] = z
-- -- -- foldr f z (x:xs) = f x (foldr f z xs)
-- const 1 (foldr const 0 [2..5])
-- const 1 _
-- 1
-- *Main> foldr const 0 [1..5]
-- 1

-- foldr (flip const) 0 [1..5]
-- (flip const 1 0) (foldr (flip const) 0 [2..5])

-- 0 _
-- 0
-- *Main> foldr (flip const) 0 [1..5]
-- 0


-- foldl (flip const) 0 [1..5]
-- -- foldl f acc [] = acc
-- -- foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldl (flip const)    ( ( flip const) 0 1 )     [2..5]

-- foldl (flip const)       ( ( flip const)   (( flip const) 0 1 )  2  )    [3..5]

-- foldl (flip const)    ( (flip const)    ( ( flip const)   (( flip const) 0 1 )  2  )   3   )       [4..5]



-- foldl (flip const)     ( (flip const)     ( (flip const)    ( ( flip const)   (( flip const) 0 1 )  2  )   3  )  4 )              [5]



-- foldl (flip const)     (  (flip const)    ( (flip const)     ( (flip const)    ( ( flip const)   (( flip const) 0 1 )  2  )   3  )  4 )    5  )     []


-- (  (flip const)    ( (flip const)     ( (flip const)    ( ( flip const)   (( flip const) 0 1 )  2  )   3  )  4 )    5  )
-- -- *Main> ( flip const) 0 1
-- -- 1
-- (  (flip const)    ( (flip const)     ( (flip const)    ( ( flip const)   (1 )  2  )   3  )  4 )    5  )
-- -- *Main> ( flip const)   (1 )  2
-- -- 2

-- (  (flip const)    ( (flip const)     ( (flip const)    ( 2  )   3  )  4 )    5  )
-- *Main> (flip const)    ( 2  )   3
-- 3

-- (  (flip const)    ( (flip const)     ( 3 )  4 )    5  )

-- *Main> (flip const)     ( 3 )  4
-- 4

-- (  (flip const)    ( 4 )    5  )
-- *Main> (flip const)    ( 4 )    5
-- 5





-- foldl const 0 [1..5]
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldl const    (const 0 1)     [2..5]
-- --foldl  f     <    acc   >    <(x:xs)>

-- foldl const    (const   (const 0 1) 2)     [3..5]
-- --foldl f      ( f      <    acc  >  x)     xs              -- this f acc x becomes the new acc to evaluate


-- --foldl f      <            acc      >     (x:xs)


-- foldl const    (const (const   (const 0 1) 2)  3   )        [4..5]


-- foldl const        ( const  (const (const   (const 0 1) 2)  3   ) 4  )         [5]



-- foldl const        (const ( const  (const (const   (const 0 1) 2)  3   ) 4  )  5 )        []     
-- --- foldl f acc [] = acc

-- (const ( const  (const (const   (const 0 1) 2)  3   ) 4  )  5 )
--                                 <eval this>                             -- due to parenthesis
-- (const ( const  (const (const   0 2)  3   ) 4  )  5 )
--                         <eval this>
-- (const ( const  (const 0  3   ) 4  )  5 )
-- (const ( const  0 4  )  5 )
-- (const 0  5 )
-- 0

-- *Main> foldl const 0 [1..5]
-- 0




--  Exercises: Understanding folds

-- 1. foldr (*) 1 [1..5]
-- -- -- -- foldr f z [] = z
-- -- -- -- foldr f z (x:xs) = f x (foldr f z xs)
-- (*) 1 (foldr (*) 1 [2..5])
-- (*) 1 ( (*) 2 (foldr (*) 1 [3..5]))

-- (*) 1 ( (*) 2 ((*) 3 (foldr (*) 1 [4..5])))
-- (*) 1 ( (*) 2 ((*) 3 ((*) 4 (foldr (*) 1 [5]))))
-- (*) 1 ( (*) 2 ((*) 3 ((*) 4 ((*) 5 (foldr (*) 1 [])))))
-- (*) 1 ( (*) 2 ((*) 3 ((*) 4 ((*) 5 (1)))))
-- (*) 1 ( (*) 2 ((*) 3 ((*) 4 (5))))
-- (*) 1 ( (*) 2 ((*) 3 (20)))
-- (*) 1 ( (*) 2 (60))
-- (*) 1 ( 120)
-- 120

-- Will return the same result as which of the following?
-- a) flip (*) 1 [1..5]
-- b) foldl (flip (*)) 1 [1..5]
-- c) foldl (*) 1 [1..5]

-- for multiplication it would not mattter foldl or foldr - so c would be the answer

-- foldl (*) 1 [1..5]
-- foldl (*)      ((*) 1 1 )    [2..5]
-- foldl (*)  ( (*)  ((*) 1 1 ) 2 )       [3..5]
-- foldl (*)  ( (*) ( (*)  ((*) 1 1 ) 2 ) 3)          [4..5]
-- foldl (*)  ( (*) ( (*) ( (*)  ((*) 1 1 ) 2 ) 3) 4 )         [5]
-- foldl (*)  ( (*) ( (*) ( (*) ( (*)  ((*) 1 1 ) 2 ) 3) 4 ) 5 )       []
-- ( (*) ( (*) ( (*) ( (*)  ((*) 1 1 ) 2 ) 3) 4 ) 5 )
-- ( (*) ( (*) ( (*) ( (*)  (1) 2 ) 3) 4 ) 5 )
-- ( (*) ( (*) ( (*) ( 2 ) 3) 4 ) 5 )
-- ( (*) ( (*) ( 6) 4 ) 5 )
-- ( (*) ( 24 ) 5 )
-- 120


-- 2. Write out the evaluation steps for:
-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*))       ( (flip (*)) 1 1)     [2..3]
-- foldl (flip (*))      ((flip (*)) ( (flip (*)) 1 1) 2 )    [3]
-- foldl (flip (*))     ( (flip (*)) ((flip (*)) ( (flip (*)) 1 1) 2 ) 3)   []
-- ( (flip (*)) ((flip (*)) ( (flip (*)) 1 1) 2 ) 3)
-- ( (flip (*)) ((flip (*)) ( 1) 2 ) 3)
-- ( (flip (*)) (2 ) 3)
-- 6




-- 5. The following are simple folds very similar to what you’ve already seen, but each has at least one error. 
-- Please fix and test them in your REPL:

-- a) foldr (++) ["woot", "WOOT", "woot"]          -- missing the initial value 
-- *Main> foldr (++) [] ["woot", "WOOT", "woot"]
-- "wootWOOTwoot"


-- b) foldr max [] "fear is the little death"          ---
-- *Main> foldr max ' ' "fear is the little death"
-- 't'

-- c) foldr and True [False, True]
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- and False (foldr and True [True])
-- and False ( and True (foldr and True []))
-- and False ( and True (True))



-- and False ( and True ( foldr and True [])
-- and False ( and True ( True) ) 

-- foldr (\x -> and (x:[])) True [False, True]

andFunc :: Bool -> Bool -> Bool
andFunc a b = and (a: b : [])
--foldr andFunc True [False, True]
-- *Main> foldr andFunc True [False, True]
-- False
-- or write lambda equivalent.
-- foldr (\a b -> and (a:b:[])) True [False, True]
-- *Main> foldr (\a b -> and (a:b:[])) True [False, True]
-- False


-- d) This one is more subtle than the previous. Can it
-- ever return a different answer?
-- foldr (||) True [False, True]      --- will always return True
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- (||) False ( foldr (||) True [True])

-- (||) False ( (||) True (  foldr (||) True []) )
-- (||) False ( (||) True (  True) )
-- (||) False ( True )
-- True

-- *Main> :t (||)
-- (||) :: Bool -> Bool -> Bool

-- *Main> foldr (||) True [False, True]
-- True
-- *Main> foldr (||) True [False, False]
-- True


-- e) foldl ((++) . show) "" [1..5]

-- foldl ((++) . show)  (  ((++) . show)   "" 1)   [2..5]

intCharComb :: [Char] -> Int -> [Char]
intCharComb str i = str ++ (show i)
--foldl (intCharComb) "" [1..5]
-- *Main> foldl (intCharComb) "" [1..5]
-- "12345"

-- foldl (\str i -> str ++ (show i)) "" [1..5]
-- *Main> foldl (\str i -> str ++ (show i)) "" [1..5]
-- "12345"



-- f) foldr const 'a' [1..5]
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- *Main> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- const 1 ( foldr const 'a' [2..5])
-- const 1 ( const 2 ( foldr const 'a' [3..5]))
-- const 1 ( const 2 ( const 3 ( foldr const 'a' [4..5])))
-- const 1 ( const 2 ( const 3 ( const 4 ( foldr const 'a' [5]))))

-- const 1 ( const 2 ( const 3 ( const 4 ( const 5 ( foldr const 'a' [])))))

-- const 1 ( const 2 ( const 3 ( const 4 ( const 5 ( 'a')))))

-- const 1 ( const 2 ( const 3 ( const 4 ( 5))))
-- const 1 ( const 2 ( const 3 ( 4)))

-- const 1 ( const 2 ( 3))
-- const 1 ( 2)

-- 1
-- *Main> foldr (flip const) 'a' [1..5]     -- what's return must be type a 
-- 'a'
-- *Main> foldr (flip const) 'a' [1,2,3,4,5, undefined]
-- 'a'



-- g) foldr const 0 "tacos"
-- *Main> foldr (flip const) 0 "tacos"
-- 0



-- h) foldl (flip const) 0 "burritos"
-- i) foldl (flip const) 'z' [1..5]




--  Unconditional spine recursion
-- xs = [1..5] ++ undefined
-- foldl const 0 xs
-- -- foldl f acc [] = acc
-- -- foldl f acc (x:xs) = foldl f (f acc x) xs
-- foldl const ( const 0 1) ([2..5]++undefined) 
-- .... due to this we need to keep recursnig till the end since its a FOLD LEFT. So the outer left Const can only be resolved when rest on the right side is resolved.

-- foldl (flip const) 0 xs
-- foldl (flip const) ( (flip const) 0 1) ([2..5]++undefined) 
-- so even flip const is same. It needs to go all the way on the spine so will error on the undefined 



-- *Main> ys = [1..5] ++ [undefined]
-- *Main> foldl const 0 ys
-- 0


-- Exercises: Database processing



data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    ]


theDatabase' :: [DatabaseItem]
theDatabase' =
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 1000
    , DbNumber 300
    , DbDate (UTCTime
             (fromGregorian 1901 5 1)
             (secondsToDiffTime 34123))
    , DbDate (UTCTime
             (fromGregorian 2022 5 1)
             (secondsToDiffTime 34123))
    ]
-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them:

dbDate1 = DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
dbNum1 = DbNumber 9001


-- Test the pattern match
filterDbNumBool :: [DatabaseItem]
              -> [Bool]
filterDbNumBool [] = []
filterDbNumBool (DbNumber x :xs) = True : filterDbNumBool xs
filterDbNumBool (x: xs) = False : filterDbNumBool xs
-- *Main> filterDbNumBool theDatabase 
-- [False,True,False,False]
 
filterDbDate :: [DatabaseItem]
              -> [UTCTime]
filterDbDate [] = []
filterDbDate ( DbDate x :xs) = x: filterDbDate xs
filterDbDate (x: xs) = filterDbDate xs
-- *Main> filterDbDate theDatabase 
-- [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC]

-- 3. Write a function that gets the most recent date:

utcList = filterDbDate theDatabase
utcList' = filterDbDate theDatabase'
-- mostRecentDate = foldr max 
greaterTest [x1,x2] = if x1 > x2 then True else False

-- mostRecent :: [DatabaseItem]
--               -> UTCTime

initTime = UTCTime (fromGregorian 1800 01 01) (secondsToDiffTime 0)

mostRecent :: [UTCTime]
              -> UTCTime
-- mostRecent [] = initTime
mostRecent [] = error "null not possible"
mostRecent [x] = x
mostRecent (x:y:xs) = if x > y then mostRecent (x:xs) else mostRecent (y:xs)


-- *Main> mostRecent utcList 
-- 1921-05-01 09:28:43 UTC

-- *Main> xx = filterDbDate []
-- *Main> xx
-- []
-- *Main> mostRecent xx
-- 1800-01-01 00:00:00 UTC

-- *Main> mostRecent xx
-- *** Exception: null not possible
-- CallStack (from HasCallStack):
--   error, called at purple-10-folding_lists.hs:545:17 in main:Main

mostRecent' :: [DatabaseItem]
              -> UTCTime
mostRecent' [] = error "null not possible"
mostRecent' dbl = max where
                  utcL = filterDbDate dbl
                  max = mostRecent utcL
-- *Main> mostRecent' theDatabase 
-- 1921-05-01 09:28:43 UTC

-- 4. Write a function that sums all of the DbNumber values:

sumDb :: [DatabaseItem]
              -> Integer
sumDb [] = 0
sumDb (DbNumber x: xs) = x + sumDb xs
sumDb (_:xs) = sumDb xs

-- *Main> sumDb theDatabase
-- 9001
-- *Main> sumDb theDatabase'
-- 10301


-- 5. Write a function that gets the average of the DbNumber values:

-- avgDb :: [DatabaseItem]
-- -> Double
-- avgDb = undefined



-- foldr toTail [] [1..5]
-- toTail 1 ( foldr toTail [] [2..5])
-- toTail 1 ( toTail 2 ( toTail 3 (toTail 4 (toTail 5 []))))


toTail :: Int -> [Int] -> [Int]
toTail i [] = []
toTail i xs = xs
-- *Main> foldr toTail [] [1..5]
-- []
-- *Main> foldr toTail [100,200] [1..5]
-- [100,200]




-- 10.9 - Scans



-- scanr (+) 0 [1..3]
-- [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
-- [6, 5, 3, 0]



-- *Main> scanr (+) 0 [1..10]
-- [55,54,52,49,45,40,34,27,19,10,0]
-- *Main> foldr (+) 0 [1..10]
-- 55


-- *Main> scanl (+) 0 [1..3]
-- [0,1,3,6]
-- *Main> foldl (+) 0 [1..3]
-- 6


-- scanl (+) 1 [1..3]
-- -- unfolding the
-- -- definition of scanl
-- = [ 1, 1 + 1
-- , (1 + 1) + 2
-- , ((1 + 1) + 2) + 3
-- ]
-- -- evaluating addition
-- = [1, 2, 4, 7]scanl (+) 1 [1..3]
-- -- unfolding the
-- -- definition of scanl
-- = [ 1, 1 + 1
-- , (1 + 1) + 2
-- , ((1 + 1) + 2) + 3
-- ]
-- -- evaluating addition
-- = [1, 2, 4, 7]
