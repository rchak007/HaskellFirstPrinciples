import Control.Monad.State
-- 53:00 - 

-- modules, packages are trivial... all these are boiler plate. not conceptual. 
-- if they named State transofrmer it woujld have been better name. State is only a function and not a state value.
-- transfomrmer is a way to combine Monads.
-- elephant in the room.. problem with Monad.

-- i was declaring these eval type things..
-- eval :: Expr -> Maybe Value
-- eval :: Expr -> [] Value    
-- eval :: Expr -> Reader Table Value
-- eval :: Expr -> State Table Value
-- eval :: Expr -> Writer Table Value
-- eval :: Expr -> IO Value
-- eval :: Expr -> Eithert Value
-- i was cheating a bit since i used Maybe but later for others i dropped the Maybe.
-- so whatever primises were made about error handling were broken. 
-- what good is Maybe if i cant use it inside Reader 
-- so how do you combine these things.. 
-- mostly many of these effects at once... 

-- we will start with maybe and IO
-- eval :: Expr -> MaybeIO Value 

-- 1:00:00

type Value = Int
data Expr
  = Lit Value
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show

type MaybeIO' a = Maybe (IO a)   --- not using this in the end.
type MaybeIO a = IO (Maybe a)   

returnMaybeIO :: a -> MaybeIO a
returnMaybeIO a = return. return $ a
-- returnMaybeIO a = return. return $ a   -- this works too

returnMaybeIO' :: a -> MaybeIO' a
returnMaybeIO' a = Just (return a)
-- returnMaybeIO a = return. return $ a    -- this also compiles..  works cause 1 return is for iO and other for Maybe - its type inference. Return can have 2 different forms. its plymorphic function. Return is also IO funcition and also Maybe function.



-- eval :: Expr -> MaybeIO Value 
-- eval (Lit v) = return v :: 
-- we will not make Maybe a monad and just the Io monad.

-- *Main> :set -XTypeApplications
-- *Main> :t return @IO
-- return @IO :: a -> IO a
-- *Main> :t return @Maybe
-- return @Maybe :: a -> Maybe a

-- the @ is used in 2 ways in haskell. 1st is in pattern matching when you make alias and 2nd one is TypeApplications

-- TypeApplications - allows you to pass a type as argument to a polymorphic function which specialised to this type.

-- this is Polymorphic while return @IO is less polymorphic. it is poly in only 1 variable.
-- *Main> :t return
-- return :: Monad m => a -> m a

bindMaybeIO' :: MaybeIO' a -> (a -> MaybeIO' b) -> MaybeIO' b 
bindMaybeIO' (Just ioa) k = return $
--   problem is with this Just. since we have no choice but go into the IO monad. The return above wraps it into Just. Just $ also would work. 
  ioa >>= \a -> 
  case k a of
    Just iob -> iob             -- this compiles too 
--    Nothing -> return Nothing ??   --- this is the problem. you are inside an IO .. and you cant extract from the io out.. 
bindMaybeIO' Nothing k = Nothing    -- this part compiles ok. 

-- lot of compile issues.. 
-- this is the problem.. IO is special and you cant go in and out of IO. all otehr monads allow you to take a peak in the internal structure. IO does not. 
-- Irfan will come up with explanation later... 
-- thats why we wnat to go IO of Maybe. So IO is outer covering. 
-- above i am risking nesting of IO. Once you are in IO ... 

-- IO monad is opaque.. like with Maybe i can extract the internal content. we cannot with IO. then you get into returning io and from io and get nested.


-- now new one with IO outside
bindMaybeIO :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b 
bindMaybeIO ioma k = do 
  ma <- ioma 
  case ma of 
    Just a -> k a
    Nothing -> return Nothing

eval :: Expr -> MaybeIO Value 
eval (Lit v) = returnMaybeIO v
eval (Add e f) = 
  -- putStrLn ("adding " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->   -- V1.0 -- does not work as `bindMaybeIO` expected MaybeIO
--   putStrLn ("adding " ++ show e ++ " and " ++ show f)  >>= \_ ->  -- this is somethign i did and that does work V1.0
  putStrLnMaybeIO ("adding " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     -- V2.0
  eval e `bindMaybeIO` \v ->
  eval f `bindMaybeIO` \w ->
  returnMaybeIO (v + w)
eval (Div e f) = 
  putStrLnMaybeIO ("Divide " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     -- V2.0
  eval e `bindMaybeIO` \v ->
  eval f `bindMaybeIO` \w ->
  maybeToMaybeIO (divide v  w)

maybeToMaybeIO :: Maybe a -> MaybeIO a
maybeToMaybeIO ma = return ma

-- *Main> eval (Lit 7)
-- Just 7

-- *Main> eval (Lit 7) >>= \x -> putStrLn ("the result is " ++ show x)
-- the result is Just 7

-- using the my V1.0 -- where i just used PutStrLen with regular bind.
-- *Main> eval $ Add (Lit 7) (Lit 9)   -- V1.0
-- adding Lit 7 and Lit 9
-- Just 16

-- but irfan went on to define a new PutStrLn that takes a string -> IO (Maybe ()); so that we dont use standard library >>=
putStrLnMaybeIO :: String -> IO (Maybe ())
-- putStrLnMaybeIO s = do      -- with do
--   putStrLn s
--   return (Just ())
putStrLnMaybeIO s = putStrLn s >>= \_ -> return (Just ())  -- w/o do 

-- *Main> eval $ Add (Lit 7) (Lit 9)    -- ver V2.0
-- adding Lit 7 and Lit 9
-- Just 16

-- we only checked IO capabilties need to still check the Maybe capabilities.
divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just $ div x y

td1 = (Div (Add (Lit 5) (Lit 7))  (Lit 4))

-- *Main> eval td1
-- Divide Add (Lit 5) (Lit 7) and Lit 4
-- adding Lit 5 and Lit 7
-- Just 3

td0 = (Div (Add (Lit 5) (Lit 7))  (Lit 0))
-- *Main> eval td0
-- Divide Add (Lit 5) (Lit 7) and Lit 0
-- adding Lit 5 and Lit 7
-- Nothing

-- why is it print it when its not going to divide?
-- same thing comes in.. when in monadic computaion seqeuence matters.. 
-- if i move the putStrLnMaybeIO to after the eval withing add/div.. in this case maye not make diff.. cause Div is last thing that is happening. 

-- V3.0 - print later 
evalP :: Expr -> MaybeIO Value 
evalP (Lit v) = returnMaybeIO v
evalP (Add e f) = 
  evalP e `bindMaybeIO` \v ->
  evalP f `bindMaybeIO` \w ->
  putStrLnMaybeIO ("adding " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     -- V3.0
  returnMaybeIO (v + w)
evalP (Div e f) = 
  evalP e `bindMaybeIO` \v ->
  evalP f `bindMaybeIO` \w ->
  putStrLnMaybeIO ("Divide " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     -- V3.0
  maybeToMaybeIO (divide v  w)
-- return (divide v  w)   -- will also work.

td2 = Add (Div (Add (Lit 5) (Lit 7))  (Lit 0)) (Lit 17)

-- *Main> evalP td2
-- adding Lit 5 and Lit 7
-- Divide Add (Lit 5) (Lit 7) and Lit 0
-- Nothing
-- Lit 17 does not appear since we print after the computation had started failing.

-- with monads we ahve been saying before there is notion of sequence , thejre is notion of time and sequential computation unlike all the otehr pure function code - there is no concept of sequence except in pattern matching.

-- now we haev abiltiy to fail and also ability to print something,. 

-- we can also get input from user and incude that in printing 

-- V4.0 - print later 
evalG :: Expr -> MaybeIO Value 
evalG (Lit v) = returnMaybeIO v
evalG (Add e f) = 
  (getLine >>= return.Just) `bindMaybeIO` \x -> 
  evalG e `bindMaybeIO` \v ->
  evalG f `bindMaybeIO` \w ->
  putStrLnMaybeIO ("adding " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     
  putStrLnMaybeIO ("we also got " ++ show x ++ " from User") `bindMaybeIO` \_ ->
  returnMaybeIO (v + w)
evalG (Div e f) = 
  evalG e `bindMaybeIO` \v ->
  evalG f `bindMaybeIO` \w ->
  putStrLnMaybeIO ("Divide " ++ show e ++ " and " ++ show f)  `bindMaybeIO` \_ ->     -- V3.0
  maybeToMaybeIO (divide v  w)

gt1 = Add (Lit 7) (Lit 5)

-- *Main> evalG gt1
-- Rafa                          -- waited for input from User
-- adding Lit 7 and Lit 5
-- we also got "Rafa" from User
-- Just 12


-- eg. keeping the state and printing
-- State + IO  
-- IO is always on the outside. 
-- if user entered INC increase, dec - decrease
example :: StateT Int IO ()
example = do 
  liftIO $ putStrLn ("Enter the instruction: ")
  instruction <- liftIO $ getLine 
  case instruction of 
    -- "i" -> return -- v1.0
    "i" -> do 
      v <- get
      put (v+1)         -- v 2.0
    -- "d" -> return () -- v1.0
    "d" -> do
      v <- get
      put (v-1)          -- V2.0
    "x" -> return ()
-- this is just demo - not doign anything.

-- v1.0
-- *Main> runStateT example 7
-- Enter the instruction: 
-- i
-- ((),7)

-- v2.0
-- *Main> runStateT example 7
-- Enter the instruction: 
-- i
-- ((),8)
-- *Main> runStateT example 7
-- Enter the instruction: 
-- d
-- ((),6)
-- *Main> runStateT example 7
-- Enter the instruction: 
-- x
-- ((),7)




example2 :: StateT Int IO ()
example2 = do 
  v <- get
  liftIO.putStrLn $ "The state is " ++ show v
  liftIO $ putStrLn ("Enter the instruction: ")
  instruction <- liftIO $ getLine 
  case instruction of 
    "i" -> do 
      put (v+1)
      example2     -- will keep running 
    "d" -> do
      put (v-1)
      example2
    "x" -> return ()


-- *Main> runStateT example2 7
-- The state is 7
-- Enter the instruction: 
-- i
-- The state is 8
-- Enter the instruction: 
-- i
-- The state is 9
-- Enter the instruction: 
-- i
-- The state is 10
-- Enter the instruction: 
-- d
-- The state is 9
-- Enter the instruction: 
-- d
-- The state is 8
-- Enter the instruction: 
-- d
-- The state is 7
-- Enter the instruction: 
-- d
-- The state is 6
-- Enter the instruction: 
-- d
-- The state is 5
-- Enter the instruction: 
-- d
-- The state is 4
-- Enter the instruction: 
-- d
-- The state is 3
-- Enter the instruction: 
-- x
-- ((),3)
-- ends at x.

-- now did i have to pass any argument to any functijons? did i have to think of keeping state separately, did i have to separately think of returning printing .. that is what Monad transformers give you. StateT when combined with IO , gives you new monad which has properties of both State and IO. This is very simple example but covers it very well. THis is as close to imperative progr in officially functional lang. 

-- *Main> :t example2
-- example2 :: StateT Int IO ()
-- example is NOT a function. it is just a State val;ue that does not take argument. 

-- what takes an argument is this--
-- this below is the function 
-- *Main> :t runStateT example2
-- runStateT example2 :: Int -> IO ((), Int)
-- implicitly is the function but the input which i am giving ... implicitly is the function.. input i am giving is the function sitting inside the example.

-- HW do with Either IO
-- find the bind and return for that.. 