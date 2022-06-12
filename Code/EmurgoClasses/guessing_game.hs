-- import System.IO.Unsafe
import Data.Char

-- main :: IO ()
-- main =
--   let x = unsafePerformIO getLine
--   in putStrLn $ "hello, " ++ x

-- (>>=) :: IO String -> (String -> IO a) -> IO a
--
-- getLine >>= \first ->
-- getLine >>= \second ->
-- putStrLn ...

-- return :: a -> IO a
-- >> :: IO a -> IO b -> IO b

-- pure code
checkGuess :: Int -> Int -> (String, Bool)
checkGuess guess secret = case compare guess secret of
    EQ -> ("Yay! You won.", False)
    LT -> ("Ah, Low! Guess again.", True)
    GT -> ("Ah, High! Guess again.", True)
-- pure code

-- impure code
getNumber :: IO Int
getNumber = getLine >>= \s -> return $ read s

processGuess :: Int -> IO ()
processGuess secret =
  putStrLn "Enter the guess number:" >>= \_ ->
  getNumber >>= \guess ->
  case checkGuess guess secret of
    (s, True) -> putStrLn s >> processGuess secret
    (s, False) -> putStrLn s

main :: IO ()
main =
  putStrLn "Enter the secret number:" >>= \_ ->
  getNumber >>= \secret ->
  processGuess secret
-- impure code
