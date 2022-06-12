import Control.Monad.State
import Control.Monad.Reader
-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/27/22 -- Friday
-----------------------------------------------------------------
-----------------------------------------------------------------


-- *Main Control.Monad.State> :i State
-- type State :: * -> * -> *
-- type State s = StateT s Data.Functor.Identity.Identity :: * -> *
    -- Defined in ‘Control.Monad.Trans.State.Lazy’

-- State Monad allow us to compose those function calls , we can make functions calls w/o state monad too of course.. 
-- basically its a function which has been Type synonym into a new type. thats a polymorphic function in 2  different variables. 
-- just by saying type State we do not get a monad. 

-- 36:20
-- if you declare Maybe you dont get a Monad.
-- likewise if you decalare a state like below its not a Monad
type State2 s a = s -> (a,s)
-- *Main> :k State2
-- State2 :: * -> * -> *
-- instance Monad (State2 s) where ...   -- just like Functor 

-- type Monad :: (* -> *) -> Constraint      
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- 40:00
-- is * -> * is a function
-- based on Kind we say -- it something thats takes a type ..its not really function.. its function at anotehr level at type level. 
-- State2 is a Function takes 2 arguments and creates a new Type.

-- Monad takes 1 argument as type constructor. State wont do.
-- so you need to fix 1 type which is s. hidden type is `a`. 
-- thats what you base your Monad on. 
-- importhing is `a` is NOT the state. but `s` is state. State you are threading through your whole computation. You dont refer to `s` directly. its mostly is hidden , whateve you are changing yuou dont need to explicitly get hold of it, its automatically passed. 
-- `a` is a value yhou may or may not choose to return meaningfully. It might represent somethign or may not. 

-- 42:24

-- type Val = Float
type Val = Int

data Expr
  = Con Val
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

-- newtype State2 s a = State2 { runState :: s -> (a,s) }
-- type State2 s a = s -> (a,s)
-- eval :: Expr -> State Int Val -- Expr -> Int -> (Val,Int)
-- when you new type its completely new type.. and its not like the synonym.. 
-- so when its type State2.... its sysnonum and its like Expr -> Int -> (Val,Int)
-- but in Haskell standard library thats now how its done. they wrap this up in another new data type. but its not operationally different when you are working. 
-- core of what define is same. you can compare with how we defined our own custom the `bind` and `return` 
-- wrapper function is State and unwrap is runState.

-- Prelude Control.Monad.State> :k Monad
-- Monad :: (* -> *) -> Constraint
-- takes a type constructor and at type level - * -> *

-- when i dont have State defined in my program then i will be using standard library - import Control.Monad.State
-- actually where the state is defined is not even defined like that.. there is anotehr level of abstraction we will talk about when we do Transformers.
-- state just happens to be a special case of more general monad related type which is a State transformer... more later..

type Times = Int 


-- here are doing expressions Add, Mul and Div, which is Val, but also we want to track how many times Div was done. Which we will store in Times. 
-- first combursome way .. w/o using state directly
eval :: Expr -> Times -> (Val, Times)
eval (Con v) =  \t -> (v, t)         -- this returns a Function 
eval (Add e f) = \t ->
  let 
    (v, t') = eval e t
-- since `e` can be another expression which could have div in it. So thats it could go from t to t' 
    (w, t'') = eval f t'
  in (v+w, t'')
-- below if you dont use let. 
-- (fst (eval e t) + (eval f (snd $ eval e t)), t'')

eval (Mul e f) = \t ->
  let 
    (v, t') = eval e t
    (w, t'') = eval f t'
  in (v*w, t'')
eval (Div e f) = \t ->
  let 
    (v, t') = eval e t
    (w, t'') = eval f t'
  -- in (v / w, t'' + 1)
  in (v `div` w, t'' + 1)
-- we have Times in eval type signature because we need initial value of how many times Div was used. Every time expression is valuated, it gives Value but also updates Times. 


-- State is most general of reader and writer, so once you get this others are straightforward. 

-- 1:01:00

x = Add ( Div (Con 12) (Con 4)) (Con 5) 
x'' = Add ( Mul (Con 12) (Con 4)) (Con 5) 
-- *Main Control.Monad.State> eval x 0
-- (8.0,1)

-- *Main Control.Monad.State> :t eval x
-- eval x :: Times -> (Val, Times)
-- we are returning a Function value. And we cant print that. We need to supply a Times value to 

-- 1:10:00

-- after changing Val to Int instead of float.
-- *Main Control.Monad.State> eval x 0
-- (8,1)

-- *Main Control.Monad.State> eval x'' 0
-- (53,0)


-- now we do the same eval with State from library.

evalS :: Expr -> State Times Val
evalS (Con v) =  return v
evalS (Add e f) = do
  v <- evalS e 
  w <- evalS f 
  return (v+w)       -- we dont refer to t (Times) at all here.
evalS (Mul e f) = do
  v <- evalS e 
  w <- evalS f 
  return (v*w)  
evalS (Div e f) = do
  v <- evalS e 
  w <- evalS f 
  modify (+ 1)
  return (div v w)  

-- 1:17:00 
-- Irfan talks about how if evalS type signature is hidden you almost dont know its doing with State. Almost looks like normal fucntion etc.. 
-- Its impossible to know its a State thing. Coz the do notation and return with bind notation is coverting everything. thats the power of constructor type classes along with the Monadic design structure. you are getting a new language for free.
-- With same syntax you can do somethign wildly different. 

-- 1:18:00
-- *Main> runState (evalS x) 0
-- (8,1)
-- *Main> runState (evalS x'') 0
-- (53,0)

-- Infinite
xInf = Div (Con 1) xInf
-- *Main> runState (evalS xInf) 0
-- (Killed
-- its stuck 


-- below just demonstrates get and put for State
evalSGP :: Expr -> State Times Val
evalSGP (Con v) =  return v
evalSGP (Add e f) = do
  v <- evalSGP e 
  w <- evalSGP f 
  return (v+w)       -- we dont refer to t (Times) at all here.
evalSGP (Mul e f) = do
  v <- evalSGP e 
  w <- evalSGP f 
  return (v*w)  
evalSGP (Div e f) = do
  v <- evalSGP e 
  w <- evalSGP f 
  s <- get         -- get the state
  put (s + 7)      -- modify the state
  return (div v w)  

-- *Main> runState (evalSGP x) 0
-- (8,7)

-- you see how easy this is. i dont even refer to the t (times) the state. I just get it at some point and do whatever you want. 

-- with haskell you get the best of both worlds.. meaning above now sort of looks like imperative programming. 


-- 1:25:00 
-- counting how many times div was used is not that useful. 

-- if you have several states you want to pass. You can only pass 1 state but thats not a problem then you just create a new data type that has all those.
-- ex.
-- data Game = Game {
--   lives:: Int
--   score:: Int
--  -- .... so on..
-- }

-- return really does not return the State. return is only returning the `a` which is Val for us and not Times. The state is happening in the background.

-- get will look like this
-- get :: State Times Times
-- get = \s -> (s,s)    -- just give back State w/o changing
-- put :: Times -> State Times Times
-- put s = \_ -> ((),s)  -- does not care about previous value. 

-- rewriting in more bind notation the Div part
-- evalSGP (Div e f) = do
--   evalSGP e >>= \v ->
--   evalSGP f >>= \w ->
--   get >>= \s ->         -- get the state
--   put (s + 7) >>= \_ ->     -- modify the state
--   return (div v w)     -- return has nothing to do with State

-- return :: a -> (s -> (a,s))
-- return a = \s -> (a,s)
-- so return as such is not doing anythign with State. If you return a, it also takes the state and passes it back as Tuple. Because you are using `modify` and `put` to change state and not return. 

-- do is sugar coat.. but bind in a very pseudo code manner is saying 
-- there is stuff here that basically says return does not act on State. 
-- 
-- 1:33:00


-- it gives this type signature only if i commented out the type signatire of evalSGP we defined.
-- *Main Control.Monad.State> :t evalSGP
-- evalSGP :: (MonadState s m, Num s) => Expr -> m Val

-- *Main> :t runState
-- runState :: State s a -> s -> (a, s)


-- *Main> runState (evalS x) 7
-- (8,8)
-- *Main> runState (evalSGP x) 7
-- (8,14)    -- we did add 7 here thats why.

-- get :: State s s
-- put :: s -> State s ()
-- modify :: (s -> s) -> State s ()

-- *Main> :t get
-- get :: MonadState s m => m s
-- *Main> :t put
-- put :: MonadState s m => s -> m ()
-- *Main> :t modify
-- modify :: MonadState s m => (s -> s) -> m ()

-- benefits will outweigh trouble of learning it.

-- but whole thing is pure. THere is no mutation. Func applications being chained. 

-- calculaters that define some constants.. 
-- pie, e, avogadro's number, Planck's constant etc. 

type ValF = Float

-- State in 1 line - the state is just threading through a value of type s and that value will be changed possibly throughtout the whole computations .

-- value of pi, or plank's constant wont change. 
-- so we have situations where you want access but not change.
-- Reader Monad. it cannot be changed.
-- then we dont need to pass it again and again ./ there is only need to pass it but not return it. 
--- setting of your program. Color scheme, bindings shortcuts , indentations etc.. those wont change throughout 1 session.. i dont need to pass it around. 

-- instead of State using Environment
-- type Environment e a = e -> (a, e) -- if it was NOT a reader...
type Environment e a = e -> a      -- only return a , dont need to return e sinced we are not changing. 
-- reader monad - function that takes e and returns a. 
type Constants = [(String, Float)]   -- we pass diff constants.
-- [("pi", 3.145), ("e", 2.718)]


data Expr'
  = Lit ValF
  | Con' String
  | Add' Expr' Expr'
  | Mul' Expr' Expr'
  | Div' Expr' Expr'


-- Cumbersome method 1st - 
evalC :: Expr' -> Environment Constants ValF
evalC (Lit v) = \c -> v
evalC (Con' s) =  \c -> 
  case lookup s c of
    Nothing -> error "Constant not found"
    Just v -> v
evalC (Add' e f) =  \c -> 
  let 
    v = evalC e c 
    w = evalC f c
  in (v + w)
evalC (Mul' e f) =  \c -> 
  let 
    v = evalC e c 
    w = evalC f c
  in (v * w)
evalC (Div' e f) =  \c -> 
  let 
    v = evalC e c 
    w = evalC f c
  in (v / w)

-- every eval call will take `c` so it can have access.


-- *Main> :t lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
constants :: Constants
constants = [("pi", 3.1415), ("e", 2.718)]
-- *Main> lookup "pi" constants
-- Just 3.1415
-- *Main> lookup "e" constants
-- Just 2.718
-- *Main> lookup "k" constants
-- Nothing


-- radius = 2 Pi 2
xRadius =  Mul' (Mul' (Lit 2) (Con' "pi")) (Lit 2)
-- *Main> 2 * 3.145 *2
-- 12.58
-- *Main> evalC xRadius constants
-- 12.566

xPiE = Mul' (Con' "pi") (Con' "e")
-- *Main> evalC xPiE constants
-- 8.538597

xW = Mul' (Con' "h") (Con' "e")
-- *Main> evalC xW constants
-- *** Exception: Constant not found
-- CallStack (from HasCallStack):
--   error, called at State-Class5-27-22.hs:287:16 in main:Main

-- this above is the Reader Monad. 


-- From Emurgo slide - - session 13 Monad 2 - 
-- instance Monad (Reader e) where
--   return = pure
--   x >>= k = 
--     Reader $ \e ->
--       let 
--         v = runReader x e
--         y = k v 
--       in runReader y e
-- there is wrappign unwrapping going on 
-- takes an e here - and e is the environment and in our case its the c (Constants)
-- runReader is there because its a newtype wrapper 
-- data Reader e a = Reader { runReader :: e -> a }
-- we did not do that sicne we were not returning an wrapped function but just a function. 
-- run it on v.. .. 
-- when we say k v.. its the next computaion.. in our case its eval f. 
-- in the let.. run reader y and pass e to it. 

-- return :: a -> (e -> a)
-- return = \e -> a    (so dont worry about e just return a)

-- in this case
-- bind :: Environment e a -> (a -> Environment e b) -> Environment e b 
-- -- it may look klike a lot. basically.. 
-- -- repleace Environment e a with e -> a 
-- bind :: 
--     e -> a 
--     (a -> e -> b)       -- function takes a and returns e b
--     e -> b         -- then return Environment e -> b meaning - ultimately its return e -> b

-- ea bind k = \e -> 
--   a = ea e in k (ea e) e 
  
-- but in the standard library you will see new type and wrapper. 
-- newtype Environment e a = Environment { runEnvironment :: e -> a}

-- 2:15:00 
-- i dont have to write the binds.. there are there. 

-- socket connetion.. whenever you calling the runReader there you supply that and then all those fucntion are dependent on that and are expecting some argument ... default argument, they dont have to refer to that default argument in their bodies.. so it becomes clearer to write.


-- Actual haskell reader method 1st - 

evalR :: Expr' -> Reader Constants ValF
evalR (Lit v) = return v
evalR (Con' s) =  do 
  c <- ask 
  case lookup s c of 
    Nothing -> error "Constant not Found"
    Just v -> return v
evalR (Add' e f) =  do
  v <- evalR e 
  w <- evalR f
  return (v + w)
evalR (Mul' e f) =  do 
  v <- evalR e 
  w <- evalR f
  return (v * w)
evalR (Div' e f) =  do
  v <- evalR e 
  w <- evalR f
  return ( v / w)


-- ask -- implementation leaving as homework
-- ask :: Reader e e 

-- *Main> :t evalR xRadius 
-- evalR xRadius :: Reader Constants ValF

-- *Main> runReader (evalR xRadius) constants 
-- 12.566
-- *Main> runReader (evalR xPiE ) constants 
-- 8.538597

-- write it and experiment instead of just reading.. 

-- some original defauly orginal argument is being passed , which you can have if you are asking for it.

-- reader --- 
-- 2:25:00
-- reader is just a name for that typre --- constants to Value that kind of function. facvt that it reads it means it has access value to that value.. it does not mean its reading it . ..  the constatns array i did not pass it intop the eval function anywhere but i still have it coz this reader has this inside it. becuase its ultimately just e to a. So i automatically have it. coz when i pass it, ultimately it gets threaded through internally by the bind and return type. 
-- so the name has nothing to do with actual reading. 
-- i can only read. There is no put auxiallary for read. 

-- this read is just like State .. there is no surpentine motiopn here.. there is normal passing around of the same constant value which is represented by this constants list here. 
-- using them very easy.. jsut call it reader.. this si the type of value you want to pass aruond..   you say ask when you need it.. 
-- thats why urging to write return, bind on your own.. 
-- basicallt write your own instance.. 

-- newtype Environment e a = Environment { runEnvironment :: e -> a}
-- instance Monad (Environment e) where 
  -- return... 
 --- (>>=) .. 
-- then you can use do notation with your Environment. also have to the ask too.
-- ask :: Environment e e 
-- ask = ...
-- HOMEWORK do this





