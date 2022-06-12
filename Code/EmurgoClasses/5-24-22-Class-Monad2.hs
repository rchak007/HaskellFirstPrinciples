

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
--- CLASS 5/24/22 -- Tuesday
-----------------------------------------------------------------
-----------------------------------------------------------------
--30:29 starts only here. 

-- applicative is like general function application happening inside the context.
-- Monad - adds ability to form arbitrarily dependent computations. abstracting over both type and type constructors. All Functors and Applicative also doing that. 
-- its just a pattern. 
-- 
-- basically you extract the `a` from context and then later do whatever you want and use `return` - because that puts it back in the context. But inside you can trasnform to anotehr type.
-- Prelude> getLine >>= (\x -> return $ "Chuck" ++ x)
-- raghavan
-- "Chuckraghavan

-- below here i just went from IO String to IO Int. 
-- Prelude> getLine >>= (\x -> return $ length x)
-- Chuck
-- 5
-- Prelude> :t (getLine >>= (\x -> return $ length x))
-- (getLine >>= (\x -> return $ length x)) :: IO Int


-- wodler is the pioneer who instroduced monads.

-- arithmetic express..
-- how can i represent that as Haskell data type. 
data Expr = Con Int 
            | Add Expr Expr
            | Div Expr Expr
            | Mul Expr Expr
            deriving (Eq, Show)
-- expression can be a constant Integer. 
-- *Main> 23
-- 23
-- *Main> 23 + 34
-- 57
-- *Main> Con 23
-- Con 23
-- *Main> Add (Con 23) (Con 34)
-- Add (Con 23) (Con 34)
evalExpr :: Expr -> Int
evalExpr (Con v) = v
evalExpr (Add e f) = (evalExpr e) + (evalExpr f)
evalExpr (Div e f) = (evalExpr e) `div`  (evalExpr f)
evalExpr (Mul e f) = (evalExpr e) *  (evalExpr f)

-- *Main> evalExpr (Con 23)
-- 23
-- *Main> evalExpr (Add 
--                    (Add (Con 23) (Con 4)) 
--                      (Con 5)
-- 32
-- *Main> evalExpr (Div (Con 12) (Con 3))
-- 4
-- where would my EvalExpr fail -- 
-- Divide by zero - failure point.
-- should be safeDiv.
-- *Main> evalExpr (Div (Con 12) (Con 0))
-- *** Exception: divide by zero

divide :: Int -> Int ->  Maybe Int 
-- at this point i am not using Maybe as Monad
divide x 0  = Nothing
divide x y = Just $ div x  y

-- *Main> divide 12 0
-- Nothing

-- now i cannot use the `divide` in evalExpr as this is Maybe. and if i change the output of evslExpr to Maybe Int other stuff will fall apart.
-- Also `divide` takes int and not Expr

eval' :: Expr -> Maybe Int
eval' (Con v) = Just v
eval' (Div e f) = 
  case eval' e of 
    Nothing -> Nothing
    Just v -> case eval' f of
      Nothing -> Nothing
      Just w -> v `divide` w
eval' (Add e f) = 
  case eval' e of 
    Nothing -> Nothing
    Just v -> case eval' f of
      Nothing -> Nothing
      Just w -> Just (v + w)
eval' (Mul e f) = 
  case eval' e of 
    Nothing -> Nothing
    Just v -> case eval' f of
      Nothing -> Nothing
      Just w -> Just (v * w)

-- see how much uglier it looks.. there is no getting out of it and we have to evaluate everything... actual calculation is lost in the forest of case and just/nothing. that is for simplest type contructor Maybe.. imagine for List, Either etc.. more of a mess in other cases. 
-- so this is the problem.
-- too much code is hard.

-- i need to find someway to chain 2 maybe computarions and ultimately make it another Maybe computation. 

-- eval' is returning Maybe.
-- the soluton is simple introduction of 2 functions - one of them is 'bind'.

bind' :: Maybe a -> (a -> Maybe b) -> Maybe b 
bind' ma k = case ma of
  Nothing -> Nothing
  Just v  -> k v

-- ma is the Maybe.. Just a
-- k v is the Maybe b

-- pearl in the shell. 
-- import Prelude hiding (return, (>>=))   -- dont import anything but hide those 2 things 
-- in this case i could have written above as below.. since >>= is not imported. 

-- Irfan did this below and hiding.. but i did as bind'
-- (>>=) :: Maybe a -> (a -> Maybe a) -> Maybe b 
-- (>>=) ma k = case ma of
--      Just v  -> k v
--      Nothing -> Nothing
               
    

return' :: a -> Maybe a
return' a = Just a
-- 1:07:28;
-- irfan did return but i did return'
-- return :: a -> Maybe a
-- return a = Just a

-- now we will write a good evalMaybe.. 
evalMaybe' :: Expr -> Maybe Int
evalMaybe' (Con v) = return v
-- whereever there is just i can just use return'
evalMaybe' (Add e f ) = 
    evalMaybe' e >>= \v -> 
    evalMaybe' f >>= \w -> 
    return (v+w)
-- >>= is real deal and do is the sugar. but we need to get used to the Bind first in the beginning. 
evalMaybe' (Mul e f ) = 
    evalMaybe' e >>= \v -> 
    evalMaybe' f >>= \w -> 
    return (v * w)
evalMaybe' (Div e f ) = 
    evalMaybe' e >>= \v -> 
    evalMaybe' f >>= \w -> 
    (v `divide` w)   
-- there is no return on Div since divide already return Maybe. 

-- *Main> evalMaybe' (Add (Con 3) (Con 4))
-- Just 7
-- 1:19:00



evalMaybe'' :: Expr -> Maybe Int
evalMaybe'' (Con v) = return v
-- whereever there is just i can just use return'
evalMaybe'' (Add e f ) = 
    evalMaybe'' e `bind'` \v -> 
    evalMaybe'' f `bind'` \w -> 
    return' (v+w)
-- >>= is real deal and do is the sugar. but we need to get used to the Bind first in the beginning. 
evalMaybe'' (Mul e f ) = 
    evalMaybe'' e `bind'` \v -> 
    evalMaybe'' f `bind'` \w -> 
    return' (v * w)
evalMaybe'' (Div e f ) = 
    evalMaybe'' e `bind'` \v -> 
    evalMaybe'' f `bind'` \w -> 
    (v `divide` w)   

-- *Main> evalMaybe'' (Add (Con 3) (Con 4))
-- Just 7
-- the 1st `bind` application start with Con 3 whuich is `e` becoming a Just 3 though `evalMaybe''`.  and 2nd bind yields Just 4.    (`evalMaybe'' (Con v) acts on it to return Just v)

-- remaining can be written as below:
-- *Main> (Just 3) `bind'` (\x -> (Just 4) `bind'` (\y-> return' (x+y)))
-- Just 7

-- error propagation
-- *Main> x = Div (Con 24) (Con 0)
-- *Main> y = Add x (Con 10)
-- *Main> evalMayb'' y
-- Nothing


-- 1:24:00

-- with Maybe we get error propagation

-----------------------------------
-- HOMEWORK - do with either;
-----------------------------------


-- reader/writer monad, state monad

-- if you are playing game, how many lives you have, ammunition you havel, which level you are etc.. where is my cursor -- all these are state.

-- how many times division occured.. or add or mult happened- how many times it happened.
-- that brings us to State 
-- first we assume we dont know state monad - so we do ugly way.. 

-- for now we disrgard Maybe to keep it simple
-- evalState :: Expr -> (Int, Int)
-- State here is how many times i divide
-- Add (Con2) (Con3 : reulst 5 State is 0 )
-- Div (Con 12) (Con3) : result is 4, state is 1
-- Div (Con12) (Div (Con 24) (Con 8)) --- state is 2

-- evalState (Add e f) = 

-- Add (Div (Con 12) (Con 3) ) (Con 7) -- State 1
-- when you play a game, a state is being manipulated every time based on Action

-- in Haskell you only have Function. 

-- ex. move only chagnes a position to new position. similarily State.

-- 1:37:58
type Point = (Int , Int)   
-- this is a position
-- you are playing snake or somewthing.. 

-- moveUp :: 
-- we need to maintain 
-- if x = (0,0) inside i cant make it (0,1)

-- in haSKELL to mutate a state you need to pass through Function.
-- in haskell its literally inpossible to change inside a pure function.
moveUp :: Point -> Point
moveUp (x,y) = (x, y+1)
moveDown :: Point -> Point
moveDown (x,y) = (x, y-1)
moveLeft :: Point -> Point
moveLeft (x,y) = (x-1, y)
moveRight :: Point -> Point
moveRight (x,y) = (x, y-1)

-- if yuou move left 5 times, 23 times up etc.. etc.. 
-- move' :: Point -> Point
-- move' (x,y) = 
--   let 
--     (x', y') = moveUp (x,y)
--     (x'', y'') = moveRight (x',y')

-- just demonstrating that i may have to do all this to do like this.

-- in c, java.
-- moveUp();
-- moveRight();
-- if position > target then moveDown()... 
-- we want that .. actually we do. lol..

-- thats what State gives us in Haskell. 

-- this point is the State. You also are returning some value going with it.. any Polymorphics types...  you also want to return Where are you from some point.. etc.. some other info other thank just State. 
-- returning some valsaue and also change somethning inside the object.

type State' s a = s -> (a,s)
-- state itself a function. Badly named. 
-- takes type s, return tuple which has value of a and s. 
-- type of a function

type Mover = Point -> Point
-- movedown 
type Direction = String

-- State Point () = Point -> ((), Point)
-- move :: Direction -> Point -> ((),Point)  
move :: Direction -> State' Point ()
-- in this case since i am only moving direction and am not returning anything else. 
move "U" (x,y) = ((), (x,y+1))
move "D" (x,y) = ((), (x,y-1))
move "L" (x,y) = ((), (x-1,y))
move "R" (x,y) = ((), (x+1,y))

-- *Main> :t (move "U")
-- (move "U") :: State Point ()
-- *Main> :t move
-- move :: Direction -> State Point ()
-- move "U" takes Point (x,y) and gives an ((), Point) becuase the type of State s a = s -> (a,s)      -- so move "U" will takes `s` which is POint and give (a,s) and here that is ((), Point)


-- now i need to define a return and bind for this State

-- State if its going to be a Monad or Functor it has to be parametric in 1 type Constructor, not 2. So i have fix the STate type. Which i have fixed here as Point. What i have not fixed is `a`. It could be anything.
-- in this case its a Void in some case it could be. 
-- Some other functonb Point could also be something else, but then it will be a different Monad.
-- just like when you do Maybe is something but when you do  either you have to fix the error type. 
-- in the same manner - State's type you have to fix. 
-- value hiding in side the context is not `s` .. s is somethign thats already.. member variable of your class and is not referred to when its you are calling the function but internally changed when the function is running. 
-- `a` is the thing that is introduced into the context.

-- return :: a -> (s -> (a,s))
returnS :: a -> State' s a
returnS a = \s -> (a,s)
-- this whole thing becomes function.
-- i am using `s` as a type variable as well as the value variable also. Its not a problem becuase these 2 living in diff name spaces. We have already seen that in Maybe example 

-- (>>=) ::
-- (>>=) :: State s a -> (a -> State s b) -> State s b 


--moveUp, then moveLeft,moveDown
movement :: State' Point ()
--movement :: Point -> ((), Point)

-- movement (x,y) =
--   move "U" (x,y) 
--   move "L" (x,y)
--   move "D" (x,y)
-- but what is the problem here - we can keep passing x,y.. x, y.. here you need to update it .

movement (x0,y0) =
  let 
    ((), (x1,y1))     =   move "U" (x0,y0) 
    ((), (x2,y2))   =   move "L" (x1,y1)
    ((), (x3,y3)) =   move "D" (x2,y2)
  in ((), (x3, y3))
-- this is the Ugly version .. like Maybe.. 

-- *Main> :t movement 
-- movement :: State Point ()
-- movement will take a Point  and give me a new point after executing the 3 movements. Up, left, down. 
-- *Main> movement (0,0)
-- ((),(-1,0))

-- you see how bad this is. Why did i not say move U. Move L like composition operator? well because i am also am geting the () value but you will realize the utility later if we return something.
-- like in future if you want to know # of divisions
-- .. in this case what if i want to return which quadrant i am in after this movement instead of returning Void.
-- we had Void for simplicity. 

-- State definition
-- you need to remember that when i say State its a function.
-- you cannot change a value in place.. you are composing functions - bigger function that does more, we are not using (.) (normal composition) but the more powerful cousin (>>=) bind operator
-- *Main> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- *Main> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- monad allows you to chain functions as well as combine values.

-- now going back to return. 
-- you always do lambda when you doing state.
-- returnS :: a -> State s a
-- returnS a = \s -> (a,s)

-- if i say return void - i will just get the state whaever was there and put the void in `a` position and return.

-- >>= :: 
-- (s -> (a,s)) ->
-- -> ( a -> (s -> (b,s)))   -- there will be a function that takes an 'a' and returns another function.
-- -> (s -> (b,s))

-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- sa >>= k = \s0 ->
--   let 
--     (a,s1) = sa s0
--     (b, s2) = (k a) s1
--   in (b, s2)


bindS :: State' s a -> (a -> State' s b) -> State' s b
sa `bindS` k = \s0 ->
  let 
    (a,s1) = sa s0
    (b, s2) = (k a) s1
  in (b, s2)

-- .. (b,s) .. this is out goal.
-- now its a game of jigsaw puzzle. we need to fit it in the pieces

-- movement had 3 moves.. here you may think its onyl 2 -- but you have to then do nested lambda's, nested binds. 

-- the bind is just saying - whatever state this gives. will take a state called s0, but state is also a function - its a name type problem . this is really a function that transforms by taking a state pass it thropugh first one get the new state , & get the value a, make a new state transfoematoipn out of that k and a, pass the newer state s1 to that and get newest state s2 and also value of type b, and return the value in a tuple. 

-- now re-writing movement with bind.

movementS :: State' Point ()
movementS =
  move "U" `bindS` \_ ->     -- lambda since we use Void we throw it
  move "L" `bindS` \_ ->
  move "D"

-- *Main> movementS (0,0)
-- ((),(-1,0))

movementS2 :: State' Point ()
movementS2 =
  move "U" `bindS` \_ ->     -- lambda since we use Void we throw it
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "L" `bindS` \_ ->
  move "D"

-- *Main> movementS2 (0,0)
-- ((),(-10,0))

-- the idea here is that all the State plumbing is done for you.

-- think about how i can fget hold of state and lets say.. 
-- remember above in the lamda \ its the value of `a` that comes and its not the State.

-- movementS3 :: State Point Quadrant
-- if lets say we weant to know which quadrant we are in
-- gets the state .. 
-- check the quadrant, 
-- return q (quadrant)


-- do notation with ;
-- movementS4 :: State Point ()
-- movementS4 = do {
--   move "U";
--   move "L";
--   move "L";
--   move "L";
--   move "L";
--   move "L";
--   move "D";
-- }
-- this will work only after changing move to conform to original defintion of State.
-- import Control.Monad.State
-- for runState

