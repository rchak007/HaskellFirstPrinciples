
-- 16.1 Functor

-- In the last chapter, on Monoid, we saw what it means to talk about an algebra and turn that into a type class.
-- This chapter, on Functor, and the two that follow, on Applicative and Monad, will be similar. 
-- Each of these algebras is more powerful than the last, but the general concept here will remain the same:
-- we abstract out a common pattern, make certain it follows some laws, give it an awesome name, and wonder how we ever lived without it.

-- Monad sort of steals the Haskell spotlight, but you can do more with Functor and Applicative than many people realize.
-- Also, understanding Functor and Applicative is important to a deep understanding of Monad.

-- Functor is all about a pattern of mapping over structure.
-- ********************************************************  map vs fmap ****************************************************
-- We saw fmap it worked just the same as map, but we also said back then that the difference is that you can use fmap with structures that aren’t lists.

-- This chapter will include:
-- • The return of the higher-kinded types.
-- • fmaps galore and not only on lists.
-- • Words about type classes and constructor classes.
-- • Puns based on George Clinton music, probably.


-- 16.2 What’s a functor?

-- A functor is a way to apply a function over or around some structure that we don’t want to alter. 
-- apply the function to the value that is “inside” some structure and leave the structure alone.
-- That’s why it is most common to introduce functor by way of mapping over lists,
-- A function is applied to each value inside a list, and the list structure remains.
-- the length of the list after mapping a function over it will always be the same.
-- No elements are removed or added, only transformed. 
-- The type class Functor generalizes this pattern so that we can use that basic idea with many types of structure, not just lists.
-- Functor is implemented in Haskell with a type class, just like Monoid.


-- looks like this:
-- class Functor f where
-- fmap :: (a -> b) -> f a -> f b

myFunc :: Int -> Int 
myFunc a = a + 10

-- *Main> fmap myFunc (Pls 5)
-- Pls 15

-- Now let’s dissect this a bit:
-- class Functor f where
--      [1]     [2]  [3]
-- fmap :: (a -> b) -> f a -> f b
-- [4]        [5]      [6]     [7]

-- 1. class is the keyword we use, as usual, to begin the definition of a type class. 
--    Functor is the name of the type class we are defining.
-- 2. Type classes in Haskell usually refer to a type. The letters themselves, as with type variables in type signatures, 
--    do not mean anything special. f is a conventional letter to choose when referring to types that have functorial structure. 
--    The f must be the same f throughout the type class definition.
-- 3. The where keyword ends the declaration of the type class name and associated types. 
--    After the where, the operations provided by the type class are listed.
-- 4. We begin the declaration of an operation named fmap.
-- 5. The argument a -> b is any Haskell function of that type (remembering that it could be an (a -> a) function for this purpose).
-- 6. The argument f a is a Functor f that takes a type argument a. That is, the f is a type that has an instance of the Functortype class.
 -- 7. The return value is f b. It is the same f from f a, while the type argument b possibly but not necessarily refers to a different type.




-- 16.3 There’s a whole lot of fmap goin’ round


-- Prelude> map (\x -> x > 3) [1..6]
-- [False,False,False,True,True,True]
-- Prelude> fmap (\x -> x > 3) [1..6]
-- [False,False,False,True,True,True]


-- The list is, of course, one type that implements the type class Functor, but it seems unremarkable when it just does the same
-- thing as map. However, List isn’t the only type that implements  Functor, and fmap can apply a function over or around any of
-- those functorial structures, while map cannot:
-- Prelude> map (+1) (Just 1)
-- • Couldn't match expected type ‘[b]’

-- -- ************************************************    fmap over other structures than list   --*****************************

-- Prelude> fmap (+1) (Just 1)
-- Just 2
-- Intriguing! What else?
-- -- with a tuple!
-- Prelude> fmap (10/) (4, 5)
-- (4,2.0)
-- -- with Either!
-- Prelude> rca = Right "Chris Allen"
-- Prelude> fmap (++ ", Esq.") rca
-- Right "Chris Allen, Esq."






-- 16.4 Let’s talk about f, baby

-- the f in the type class definition for Functor must be the same f throughout the entire definition,
-- and it must refer to a type that implements the type class. This section details the practical ramifications of those facts.


-- ************************************************    f must have the kind * -> *   --*****************************

-- a type constant or a fully applied type has the kind *. A type with kind * -> * is awaiting application to a type constant of kind *.

-- 1. Each argument (and result) in the type signature for a function must be a fully applied type. 
--    Each argument must be kind *.
-- 2. The type f is applied to a single argument in two different places: f a and f b. 
--    Since f a and f b must each have the kind *, f by itself must be kind * -> *.


-- Shining star come into view
-- Every argument to the type constructor of -> must be of kind *.
-- verify this simply by querying the kind of the function type constructor for ourselves:

-- Prelude> :k (->)      - this is function basically
-- (->) :: * -> * -> *

-- Each argument and result of every function must be a type constant, not a type constructor.


-- Given that knowledge, we can know something about Functor from the type of fmap:

-- class Functor f where
--      fmap :: (a -> b) -> f a -> f b
--                  1        2      3
-- -- 1, 2, and 3 all have the kind *

-- ************************************************   good kind practice  --*****************************

-- The type signature of fmap tells us that the f introduced by the class definition for Functor must accept a single type
--    argument and thus be of kind * -> *. We

-- class Sumthin a where
-- s :: a -> a
-- class Else where
-- e :: b -> f (g a b c)
-- class Biffy where
-- slayer :: e a b
--     -> (a -> c)
--     -> (b -> d)
--     -> e c d

-- My guesses: e :: *->*->* , a and c are just *, b and d are also just *

-- Let’s deconstruct the previous couple of examples:

-- class Sumthin a where
-- s :: a -> a
-- -- [1] [1]
-- 1. The argument and result type are both a. There’s nothing
-- else, so a has kind *.


-- class Else where
-- e :: b -> f (g a b c)
-- -- [1] [2] [3]
-- 1. This b, like a in the previous example, stands alone as the
-- first argument to ->, so it is kind *.

-- 2. Here, f is the outermost type constructor for the second argument (the result type) of ->. It takes a single argument,
-- the type g a b c wrapped in parentheses. Thus, f has kind * -> *.

-- 3. And g is applied to three arguments a, b, and c. That means it is kind * -> * -> * -> *, where:
-- -- using :: to denote kind signature
-- g :: * -> * -> * -> *
-- -- a, b, and c are each kind *
-- g :: * -> * -> * -> *
-- g a b c (g a b c)



-- class Biffy where
-- slayer :: e a b
-- -- [1]
-- -> (a -> c)
-- -- [2] [3]
-- -> (b -> d)
-- -> e c d
-- 1. First, e is an argument to ->, so the application of its arguments must result in kind *. Given that, 
-- and knowing there are two arguments, a and b, we can determine e is kind * -> * -> *.

-- 2. This a is an argument to a function that takes no arguments itself, so it’s kind *
-- 3. The story for c is identical to a, just in another spot of the same function.


-- The kind checker is going to fail on the next couple of examples:
-- class Impish v where
-- impossibleKind :: v -> v a
-- class AlsoImp v where
-- nope :: v a -> v

-- Remember that the name of the variable before the where in a type class definition binds the occurrences of that name throughout the definition


-- Just as GHC has type inference, it also has kind inference.
-- And just as it does with types, it can not only infer the kinds but also validate that they’re consistent and make sense.


-- Exercises: Be kind

-- 1. What’s the kind of a?
-- f :: a -> a
-- answer a :: *
-- 2. What are the kinds of b and T? (The T is capitalized on
-- purpose!)
-- f :: a -> b a -> T (b a)
-- Answer  b :: * -> * 
--   T :: * -> *

-- 3. What’s the kind of c?
-- f :: c a b -> c b a
-- Answer c :: * -> * -> *

-- A shining star for you to see



-- ************************************************   Functor instance error example -   --*****************************
--- Let’s try it with a type constant and see what happens:

-- -- functors1.hs
-- data FixMePls =
-- FixMe
-- | Pls
-- deriving (Eq, Show)
-- instance Functor FixMePls where
-- fmap =
-- error
-- "it doesn't matter, it won't compile"

-- Notice there are no type arguments anywhere—everything is one shining (kind) star! And if we load this file from GHCi,
-- we’ll get the following error:
-- Prelude> :l functors1.hs
-- • Expected kind ‘* -> *’, but ‘FixMePls’
-- has kind ‘*’
-- • In the first argument of ‘Functor’,
-- namely ‘FixMePls’

-- But FixMePls doesn’t take type arguments,

-- Functor is function application

-- We just saw how trying to make a Functor instance for a type constant means you have function application. But, in fact,
-- fmap is a specific sort of function application. Let’s look at the types:

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- There is also an infix operator for fmap. If


-- ************************************************   <$>   --*****************************
---- <$> is the infix alias for fmap:
-- (<$>) :: Functor f
-- => (a -> b)
-- -> f a
-- -> f b
-- Notice something?
-- (<$>) :: Functor f
-- => (a -> b) -> f a -> f b
-- ($) :: (a -> b) -> a -> b

-- Functor is a type class for function application “over” or “through” some structure f that we want to ignore and leave untouched. 
-- We’ll explain “leave untouched” in more detail later, when we talk about the Functor laws.


-- A shining star for you to see what your f can truly be

-- Let’s resume our exploration of why we need a higher-kinded f.
-- If we add a type argument to the datatype from above, we make FixMePls into a type constructor, and this will work:
-- -- functors2.hs
-- data FixMePls a =
-- FixMe
-- | Pls a
-- deriving (Eq, Show)
-- instance Functor FixMePls where
-- fmap =
-- error
-- "it doesn't matter, it won't compile"
-- Now, it’ll compile!



-- But wait, we don’t need the error anymore! Let’s fix that Functor instance:

-- functors3.hs
data FixMePls a =
    FixMe
    | Pls a  deriving (Eq, Show)

instance Functor FixMePls where       --- defining the Functins (like f below) to be a functor
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

-- Let’s see how our instance lines up with the type of fmap:
-- fmap :: Functor f
-- => (a -> b) -> f a -> f b
-- fmap      f       (Pls a) = Pls (f a)
-- --     (a -> b)     f a         f b

-- Prelude> fmap (+1) (Pls 1)    ---- Here (Pls 1)  is Functor's value
-- Pls 2                         -- Is also Functor value  - or 

-- Notice that the function gets applied over and inside of the structure. This is how Haskell coders lift big heavy functions
-- over abstract structure!

-- Mine - below i tried error one
-- *Main> fmap (+1) (Pls 'a')

-- <interactive>:3:7: error:
--     • No instance for (Num Char) arising from a use of ‘+’
--     • In the first argument of ‘fmap’, namely ‘(+ 1)’
--       In the expression: fmap (+ 1) (Pls 'a')
--       In an equation for ‘it’: it = fmap (+ 1) (Pls 'a')

-- Mine - function string to string works
-- *Main> fmap (++ "b") (Pls "a")
-- Pls "ab"

-- Mine - 
-- *Main> fmap (fmap (\x->x+2)) (Pls [1,2,3])
-- Pls [3,4,5]

-- Mine
-- *Main> fmap (fmap (\x->x+2)) (Pls (Just 1))
-- Pls (Just 3)




-- ************************************************   Functor instance another error example -   --*****************************
-- OK, let’s make another mistake for the sake of being explicit.
-- What if we change the type of our Functor instance from FixMePls to FixMePls a?

-- data FixMePls a =
-- FixMe
-- | Pls a
-- deriving (Eq, Show)
-- instance Functor (FixMePls a) where
-- fmap _ FixMe = FixMe
-- fmap f (Pls a) = Pls (f a)

-- But now that argument is part of the f structure. If we compile this ill-conceived code:
-- • Expected kind ‘* -> *’, but ‘FixMePls a’
-- has kind ‘*’
-- • In the first argument of ‘Functor’,
-- namely ‘(FixMePls a)’
-- In the instance declaration for
-- ‘Functor (FixMePls a)’



-- Type classes and constructor classes

-- You may have initially paused on the type constructor f in the definition of Functor having kind * -> *, but this is completely
-- natural! In fact, earlier versions of Haskell didn’t have a facility for expressing type classes in terms of higher-kinded types
-- developed by Mark P. Jones1
-- This work generalized type classes from being usable only with types of
-- kind * (also called type constants) to being usable with higher-kinded types, called type constructors, as well.

-- In Haskell, the two use cases have been merged, such that we don’t call out constructor classes as being separate from
-- type classes, but we think it’s useful to highlight that something significant has happened here. Now we have a means of talking
-- about the contents of types independently from the type that structures those contents. That’s why we can have something
-- like fmap that allows us to alter the contents of a value without altering the structure (a list, or a Just) around the value.




-- 16.5 Functor laws


data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

-- super NOT OK
instance Functor CountingBad where
    fmap f (Heisenberg n a) =
-- (a -> b)   f          a =
        Heisenberg (n+1) (f a)
--     f            b

-- Prelude> u = "Uncle"
-- Prelude> oneWhoKnocks = Heisenberg 0 u
-- Prelude> fmap (++" Jesse") oneWhoKnocks
-- Heisenberg 1 "Uncle Jesse"
-- Prelude> f = ((++" Jesse").(++" lol"))
-- Prelude> fmap f oneWhoKnocks
-- Heisenberg 1 "Uncle lol Jesse"





-- lmls ~ List (Maybe (List String))
-- replaceWithP = const 'p'
-- Prelude> ha = Just ["Ha", "Ha"]
-- Prelude> lmls = [ha, Nothing, Just []]    -- [Just ["Ha", "Ha"], Nothing, Just []]     - p, Just p, Just ["pp"], Just ["pp","pp"], Nothing, 
-- (fmap . fmap) replaceWithP lmls

-- 1 fmap - [p,p,p]
-- 2 fmap.fmap - [Just p, Nothing , Just p]

-- 3 fmap fmap fmap - [Just "pp", Nothing , Just 'p']

-- *Main> (fmap.fmap.fmap) replaceWithP lmls
-- [Just "pp",Nothing,Just ""]

-- 4 fmap fmap fmap fmap 
-- [Just]




-- Exercises: Heavy lifting
-- Add fmap, parentheses, and function composition to each expression as needed for the expression to type check and produce
-- the expected result. It may not always need to go in the same place, so don’t become complacent:

-- 1. a = (+1) $ read "[1]" :: [Int]
-- Expected result:
-- Prelude> a
-- [2]
--  ****************** MY Answer -- 
-- Prelude> fmap (+1) $ read "[1]" :: [Int]
-- [2]

-- 2. b = (++ "lol") (Just ["Hi,", "Hello"])
-- Prelude> b
-- Just ["Hi,lol","Hellolol"]
--  ****************** MY Answer -- 
-- Prelude> (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Just ["Hi,lol","Hellolol"]



-- 3. c = (*2) (\x -> x - 2)
-- Prelude> c 1
-- -2
--  ****************** MY Answer -- 
-- Prelude> c = fmap (*2) (\x -> x - 2)
-- Prelude> c 1
-- -2


-- 4. d =
-- ((return '1' ++) . show)
-- (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"
--  ****************** MY Answer -- 
-- Prelude> d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"

-- Prelude> (\x -> [x, 1..3]) 0
-- [0,1,2,3]
-- Prelude> show((\x -> [x, 1..3]) 0)
-- "[0,1,2,3]"
-- Prelude> (return '1' ++) "[1,2,3,4]"
-- "1[1,2,3,4]"


-- 5. e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
-- changed = read ("123"++) show ioi
-- in (*3) changed
-- Prelude> e
-- 3693



-- 16.8 Transforming the unapplied type argument

-- instance Functor (Two a) where
-- fmap f (Two a b) = Two $ (f a) (f b)    

-- ******************************************  you cannot apply f a since a is already part of the Functor ******************************
-- This won’t fly, because the a is part of the functorial structure (the f). 
-- We’re not supposed to touch anything in the f referenced in the type of fmap, so we can’t apply the function
-- (named f in our fmap definition) to the a, because the a is now untouchable:



-- 16.9 QuickChecking Functor instances


-- Functor laws are the following:
-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)


-- We can turn those into the following QuickCheck properties:
-- functorIdentity :: (Functor f, Eq (f a)) =>
--         f a
--      -> Bool
-- functorIdentity f =
--     fmap id f == f
-- functorCompose :: (Eq (f c), Functor f) =>
--        (a -> b)
--      -> (b -> c)
--      -> f a
--      -> Bool
-- functorCompose f g x =
--    (fmap g (fmap f x)) == (fmap (g . f) x)





main :: IO ()
main = do 
    putStrLn (" *** " )  --
    




