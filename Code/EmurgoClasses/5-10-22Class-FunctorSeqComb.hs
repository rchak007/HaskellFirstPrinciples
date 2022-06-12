{-# LANGUAGE TypeApplications #-}
import Data.List
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.State

import System.IO.Unsafe
-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/10/22 --from here i think
-----------------------------------------------------------------
-----------------------------------------------------------------
----- putStrLn
-- Prelude> :t putStrLn
-- putStrLn :: String -> IO ()
-- this takes String in code and prints on screen and with return type IO ()
-- whereas getLine takes a string as input from User to give IO String
-- Prelude> :t getLine
-- getLine :: IO String
-- Prelude> putStrLn "one"
-- one
-- Prelude> :t putStrLn "one"
-- putStrLn "one" :: IO ()

xPut1 = putStrLn "one" >>= (\_ -> putStrLn "two")


-- we dont use \x because it anyway return IO () so we cant use it.
-- but in getLine return IO String so we can use that String to do things.
-- *Main> xPut1
-- one
-- two
-- *Main> :t xPut1
-- xPut1 :: IO ()

-- Sequence Combinator
xPut2 = putStrLn "one" >> putStrLn "two"
-- *Main> xPut2
-- one
-- two
-- So since you did not need the \x part.. this sequence Combinator just makes the writing easier.. you can just sequence them.
-- 33:37


func1 :: Maybe Int -> Int
func1 Nothing = 0
func1 (Just i) = i
-- *Main> x = Just 25
-- *Main> func1 x
-- 25

-- unwrap from Maybe
-- *Main Data.Maybe> fromJust x
-- 25

-- *Main Data.Maybe> fromJust Nothing
-- *** Exception: Maybe.fromJust: Nothing

-- 1:30:00
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b 
-- you dont want to return 'b' but return Maybe so i can propogate the error
--mapList also does the same thing. 
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- *Main> mapMaybe (^2) $ safeHead [7,8,9]
-- Just 49
-- *Main> mapMaybe (^2) $ safeHead []
-- Nothing

-- These looks similar - 
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapList :: (a -> b) -> List a -> List b
-- So how can i abstract it out

-- if there was no Num class
-- (+) @Int :: Int -> Int -> Int
-- (+) @Float :: Float -> Float -> Float

-- Num class
-- (+) :: a -> a -> a

-- Eq class
-- (==) @Char :: Char -> Char -> Char
-- (==) @Bool :: Bool -> Bool -> Bool

-- (==) :: a -> a -> a

-- map @Maybe :: (a -> b) -> Maybe a -> Maybe b
-- map @List :: (a -> b) -> List a -> List b

-- fmap  :: (a -> b) -> f a -> f b   -- f is type constructor.

-- map abstracts over a Type Contructor
-- whereas Num and Eq abstract over a type

-- Prelude> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- Prelude> :i Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
-- f is kind * -> *   - this is most importhing to realize

-- So we already have Functor so we dont need to do mapMaybe
-- *Main> fmap (^2) $ safeHead [7,8,9]
-- Just 49
-- *Main> fmap (^2) $ safeHead []
-- Nothing

--1:45:00 to 2:00:00 w.o stopping.
-- fmap over list.
-- Prelude> fmap (^2) [7,8,9]
-- [49,64,81]
-- map is only over list. fmap is Functor
-- Prelude> :t fmap @IO
-- fmap @IO :: (a -> b) -> IO a -> IO b

-- Prelude> :t fmap @IO
-- fmap @IO :: (a -> b) -> IO a -> IO b
-- can use fmap over IO too.

-- Prelude> getLine >>= (\s-> let x = read s in putStrLn.show $ x*x)
-- 7
-- 49
-- getLine gets string and convert to Int with read. Then putStrLn 
-- Prelude> putStrLn(show 49)
--49
-- putStrLn cannot print Int. So we do `show` first which converts to string
-- Prelude> :t show
-- show :: Show a => a -> String

-- 1:52:00 - how to get number through function
-- instead of getLine on Prelude we call a function that getNumber and use the return value which is IO Int already


ioi = readIO "1" :: IO Int
getNumber :: IO Int
getNumber = getLine >>= (\s -> return $ read s)
-- *Main> :t return
-- return :: Monad m => a -> m a
-- *Main> getNumber >>= (\x -> putStrLn.show $ x * x)
-- 7
-- 49

-- now how to fmap over this.
-- fmap :: (a->b) -> IO a -> IO break
getNumber' :: IO Int
getNumber' = fmap read getLine
-- *Main> getNumber' >>= (\x -> putStrLn.show $ x * x)
-- 7
-- 49

-- monad is something thats Functor as well.

-- data Either a b = Left a | Right b
-- like a tuple. forms from 2 differetn data type
-- *Main> :k Either
-- Either :: * -> * -> *
-- *Main> :k []
-- [] :: * -> *
-- *Main> :k IO
-- IO :: * -> *

-- *Main> Left "Error File not Found" :: Either String Int
-- Left "Error File not Found"
-- so that could be represented as error

-- simplest example of a sum type that is polymorphic
-- Bool is polymorphic.
-- Either is also a Functor. 
-- Chuck -- since data constructior is * -> * which is what a Functor is. so Either is a Functor too even thought the type contructor needs 2 argument.. So `Either a` would be a Functor. since that needs `b` to complete it.


-- instance Functor (Either a) where
-- -- fmap :: (b ->c) -> Either a b -> Either a c
--   fmap f (Left x) = Left x ---- we dont touch it like Nothing
--   fmap f (Right x) = Right (f x)    -- this is similat to Just
-- -- by doing this we dont touch the Left side. 

-- fmap @(Either String)
--   :: (a -> b) -> Either String a -> Either String b

-- Either becomes the wrapper.
-- unlike Maybe my errro message can also have a type


-- *Main> fmap (^2) $ Left "hello,World"
-- Left "hello,World"    -- wont apply on the Left.. 
-- *Main> fmap (^2) $ Right 23     -- Only applied on Right
-- Right 529

-- same as Tuple - only applies on 2nd part
-- *Main> fmap (^2) (2,3)
-- (2,9)

-- *Main> fmap (\Nothing -> "chlorine") $ Nothing
-- Nothing -- because fmap of Maybe is designed like that

safeHeadEither :: [a] -> Either String a
safeHeadEither [] = Left "Error: File not found"
safeHeadEither (x:xs) = Right x

-- *Main> fmap (^2) $ safeHeadEither [7,8,9]
-- Right 49
-- *Main> fmap (^2) $ safeHeadEither []
-- Left "Error: File not found"

