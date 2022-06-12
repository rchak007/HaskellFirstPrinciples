import Control.Monad.Writer
-- its when you write it and get stuck is when you learn.
-- Home work - whatever we did do on your own. Give some exampele to yourself. 

-- State 
-- s -> (a, s)
-- Reader
-- e -> a      -- reader is special case of State. you dont return a new state - since you cannot change it. No mutation - like pi, e. Special limited case of State computation. 
-- Writer -- another limited case of State computation. you put a limitation on how you can change the State.
-- (a,w)
-- you are not taking a `w` but within the computaion you have some limited capability to mutate the `w`. thats why its not a Function. hyou are changing the `w` in a very constrained manner. 

-- ex of Writer Monad.

type Value = Int
data Expr 
  = Lit Value
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

eval' :: Expr -> Value
-- this is now w/o using monad. more cumbersom approach
eval' (Lit v) = v 
eval' (Add e f) = eval' e + eval' f 
eval' (Mul e f) = eval' e * eval' f 
-- not be in IO Monad. You are loggin stuff but want to do in Pure manner and not impure manner. By adding a variable. lets say String. 
-- print we can only do with IO. but here are doing in pure manner.

-- if i do putStrLn ....
evalPut :: Expr -> IO Value
-- this is now w/o using monad. more cumbersom approach
evalPut (Lit v) = return v 
evalPut (Add e f) = do 
  putStrLn $ "added" ++ " " ++ show e ++ " and " ++ show f
  v <- evalPut e  
  w <- evalPut f 
  return (v+w)
evalPut (Mul e f) = do 
  v <- evalPut e 
  w <- evalPut f
  return (v * w)

-- *Main> evalPut (Add (Lit 2) (Lit 3))
-- added Lit 2 and Lit 3
-- 5
-- since we have putStrLn first then evaluation

-- but we dont want to use PutStrLn.. 

-- Writer monad
eval :: Expr -> Writer String Value
-- this is now w/o using monad. more cumbersom approach
eval (Lit v) = do 
  tell $ "return" ++ " " ++ show v ++ "\n"
  return v 
eval (Add e f) = do 
  tell $ show e ++ " + " ++ show f ++ "\n"
  v <- eval e  
  w <- eval f 
  return (v+w)
eval (Mul e f) = do
  tell $ show e ++ " * " ++ show f ++ "\n"
  v <- eval e  
  w <- eval f 
  return (v*w)

-- Writer is like a state?reader ... it is polymorphic monad and has 2 type contrusctors but onyl 1 of them is parameter. I have to fix one of them to show what is my type of log. 
-- because this is monad and using monadic operations i can straightaway use it. 
-- as soon as i add  import Control.Monad.Writer

-- *Main> eval (Add (Lit 2) (Lit 3))
-- WriterT (Identity (5,""))
-- but has empty "" string. (output before adding tell)
-- dont worry about WriterT for now. Itws wrapped in another data contructor in transformers. 
-- i cant use putStrLn but i can use tell. 

-- *Main> runWriter $ eval (Add (Lit 2) (Lit 3))
-- (5,"Lit 2 + Lit 3")
-- in the tuple 1st one is actual value, 2nd part is the log. (before adding more log \n and return)

wa = Mul ( Add (Lit 3) (Lit 4) ) (Lit 5)

-- *Main> runWriter $ eval wa
-- (35,"Add (Lit 3) (Lit 4) * Lit 5\nLit 3 + Lit 4\nreturn 3\nreturn 4\nreturn 5\n")
-- *Main> fst.runWriter $ eval wa
-- 35
-- *Main> putStrLn.snd.runWriter $ eval wa
-- Add (Lit 3) (Lit 4) * Lit 5
-- Lit 3 + Lit 4
-- return 3
-- return 4
-- return 5


wa2 = Add ( Add (Lit 3) (Lit 4) ) (Lit 5)
-- *Main> putStrLn.snd.runWriter $ eval wa2
-- Add (Lit 3) (Lit 4) + Lit 5
-- Lit 3 + Lit 4
-- return 3
-- return 4
-- return 5

-- *Main> fst.runWriter $ eval wa2
-- 12

-- prints outward to inwards.


-- i will implement w/o Monad.Homework for you will be to then take the our implementation and take some hint from that and try to implement with bind and return.
-- old fashion bad way
evalC :: Expr -> (Value, String)
-- this is now w/o using monad. more cumbersom approach
evalC (Lit v) = ( v, "" ++"return" ++ " " ++ show v ++ "\n")
evalC (Add e f) = 
  let 
    (v, s1) = evalC e    -- s1 is the log
    (w, s2) = evalC f    -- s2 is log from evalC f
  in ( (v + w), s1 ++ s2 ++ show e ++ " + " ++ show f ++ "\n")
evalC (Mul e f) =  
--   
  let 
    (v, s1) = evalC e    -- s1 is the log
    (w, s2) = evalC f    -- s2 is log from evalC f
  in ( (v * w), s1 ++ s2 ++ show e ++ " * " ++ show f ++ "\n")
-- note this is diff from State monad, we re passing a state now we are not. So we have just tuple now. 
-- problem when you call recursivlety since i cannot add 2 tuples. i cant do evalC e + evalC f.
-- i will write a function that reutrn tuple. get my actual value and string separately. 


-- *Main> evalC wa2
-- (12,"return 3\nreturn 4\nLit 3 + Lit 4\nreturn 5\nAdd (Lit 3) (Lit 4) + Lit 5\n")
-- *Main> fst $ evalC wa2
-- 12

-- *Main> putStrLn $ snd $ evalC wa2
-- return 3
-- return 4
-- Lit 3 + Lit 4
-- return 5
-- Add (Lit 3) (Lit 4) + Lit 5
-- we are evaluating first then printing here so order is reversed.


-- Change the order 
evalCR :: Expr -> (Value, String)
-- this is now w/o using monad. more cumbersom approach
evalCR (Lit v) = ( v, "return" ++ " " ++ show v ++ "\n" ++ "" )
evalCR (Add e f) = 
  let 
    (v, s1) = evalCR e    -- s1 is the log
    (w, s2) = evalCR f    -- s2 is log from evalC f
  in ( (v + w),  show e ++ " + " ++ show f ++ "\n" ++ s1 ++ s2 )
evalCR (Mul e f) =  
--   
  let 
    (v, s1) = evalCR e    -- s1 is the log
    (w, s2) = evalCR f    -- s2 is log from evalC f
  in ( (v * w), show e ++ " * " ++ show f ++ "\n" ++ s1 ++ s2 )

-- *Main> putStrLn $ snd $ evalCR wa2
-- Add (Lit 3) (Lit 4) + Lit 5
-- Lit 3 + Lit 4
-- return 3
-- return 4
-- return 5

-- just chnnging the order shows us evalauation strategies. 
-- outwards to inwards - follow sHaskell way. evalCR 
-- evalC is other way.. if you look at logging.
-- log of the arguments are returned before the log of the function. --- evalC
-- whereas evaCR return logs of the function is returned before the log of the argument. 

-- 
-- newtype Writer w a = { runWriter :: (a,w)}
-- instance Monad (Writer w) where
--   return = .. .. (a,w)
--   (>>=) = .. 
-- can i use any `w` here? 
-- mempty 
-- if you return = (a, "world") -- you dont want to do that because you dont want "world" ebvery time there is return. 
-- what is write is not string but a character.. then do i have a memty type of character.. 
-- then we need to also be able to combine if there is w1 and w2 .. ex.. mappend. 
-- for thewse reasons we need a monoid here. Monoid has these properties where 2 things can be combined and a defauly mempty is there.. that gives you a Write 
-- SO DO THIS HOMEWORK -----------

-- these monads we did. 
-- maybe 
-- either -- was as homework.
-- list 
-- IO 
--- there are lot more other monads.
-- reader
-- state
-- Write
-- even though we haev library.... 
-- like javascript.. learning languagte itself is no good. because domain of JS is web development, yhou need to learn to interact with web pages, manipulate web page, html, talk to network etc. when you learn JS.
-- same with hasjkell.. haskell is not domain specific but these are design patterns if you will which are used in here. they are properly absteracted.. but you need to put lot of effort when you are learning them. Using them will come as 2nd nature. 
-- will improve your workflow several times.. 
-- many haskell applications can be using these monads.
-- when you submit project you dont need to use this.. things take time so Irfan does not expect to know all. 

-- even these are not directly used -- but abstracted more.


