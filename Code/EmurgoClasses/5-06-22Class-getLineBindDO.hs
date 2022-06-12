
-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
--- CLASS 5/6/22 --
-----------------------------------------------------------------
-----------------------------------------------------------------

-- getName :: () - String
-- getName () = unsafePerformIO getLine


-- main = 

-- 5/6/22 class --- IO 
  -- getLine >>= 
  --    (\first -> 
  --        getLine >>= (\second -> 
  --            putStrLn $ "hello, " ++ first ++ " and " ++ second))
  
-- -- You dont need brackets like above
--   getLine >>=  \first -> 
--   getLine >>=  \second -> 
--   putStrLn $ "hello, " ++ first ++ " and " ++ second

-- illsutrating the >>= 
-- (>>=) :: IO String -> (String -> IO a) -> IO a
-- So basically it takes an IO of 1 type to another IO type - 
-- that way you are getting input and transforming it.



-- 5/6/22 - do is not a fundamental thing in haskell, its just a notation.
    --    do
    -- first <- getLine
    -- second <- getLine
    -- putStrLn $ "hello, " ++ first ++ " and " ++ second
    -- putStrLn "Enter another string for toUpper: "
    -- getLine >>= (\s -> putStrLn $ map toUpper s)
-- *Main> main
-- Chuck
-- Raghavan
-- hello, Chuck and Raghavan
-- Enter another string for toUpper: 
-- rafa nadal
-- RAFA NADAL


-- main =  getLine >>= 
--   (\first -> 
--     getLine >>= (\second -> 
--        putStrLn $ " Hello " ++ first ++ " and " ++ second))
-- dont need the above brackets. and also aligning it more properly
-- main =  
--     getLine >>= \first -> 
--     getLine >>= \second -> 
--     putStrLn $ " Hello " ++ first ++ " and " ++ second
-- now moving above to do structure

-- do is just syntactic sugar. do is not function its a syntactic conveniance - so the below converts to the above when its run
-- main = do
--     first <- getLine 
--     second <- getLine
--     putStrLn $ " Hello " ++ first ++ " and " ++ second
-- it looks like as thought first is being extract from getLine 
-- but what its really doing is its getting an arbitraty function to run on that and first is just lambda.


-- see guessingGame.hs
-- return 



--    putStrLn (show 23)  -- needs show to convert to String
-- *Main> :t show
-- show :: Show a => a -> String

-- takes user input and 
    -- let 
    --    x = getline    -- getline its not really a string
    --   in putStrLn(" Hello , " ++ x)


-- 31:11



-- look at also -- 
-- guessingGame.hs
-- 6-3-22-SimpleDoBind.hs