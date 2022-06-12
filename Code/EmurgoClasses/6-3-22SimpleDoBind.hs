import Data.Char

-- main = do 
--   s <- getLine
--   putStrLn s
-- *Main> main
-- Rafa
-- Rafa
gb1 = "world"


main = 
  getLine >>= \s ->
  let g = s ++ " hello "
      gU = fmap toUpper g 
  in (putStrLn gU) >>= \_ ->       -- putStrLn gU give us IO (), but i use another bind to continue to next lines...  a DO notation is just syntactic sugar so i dont have to do all this. Also i use \_ coz its putStrLn there is nothing to extract.
  putStrLn "rafa" >>= \_ ->
  let x = "blah blah " 
      y = fmap toUpper x
  in (putStrLn y) >>= \_ ->
  putStrLn ("at the end " ++ s ++ " " ++ g) 

-- basic funda --- since Main is IO () 
-- and if i dont use DO construct i can only use >>= and i need to keep stringing it together since i keep taking the IO () and then keep writing something that retulst in IO () and keep stringing it like that.. 
-- also these let variables also are retained till the end.

-- *Main> main 
-- rafa
-- RAFA HELLO 
-- rafa
-- BLAH BLAH 
-- at the end rafa rafa hello 


--below is with DO notation

mainDO :: IO ()
mainDO = do 
  s <- getLine
  let g = s ++ " hello "
  let gu = fmap toUpper g
  putStrLn gu
  let x = "blah blah "
  let y = fmap toUpper x
  putStrLn y
  putStrLn ("at the end " ++ s ++ " " ++ g)
-- *Main> mainDO
-- rafa
-- RAFA HELLO 
-- BLAH BLAH 
-- at the end rafa rafa hello 


