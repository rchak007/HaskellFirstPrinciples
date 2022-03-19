import Data.List.NonEmpty
import Data.Semigroup 


-- data NonEmpty a = a :| [a]

-- **************************************************** :| infix data constructor  ******************************************************
-- Here :| is an infix data constructor that takes two (type) arguments. It’s a product of a and [a]. 
-- It guarantees that we always have at least one value of type a, which [a] does not guarantee, as any list might be empty.

-- Because  NonEmpty is a product of two arguments, we could’ve also written it as:
newtype NonEmpty1 a =
    NonEmpty1 (a, [a])
    deriving (Eq, Ord, Show)

prodNE1 = 1 :| [2,3,4]
prodNE2 = 5 :| [6,7,8]

prodNE3 = prodNE1 <> prodNE2

main :: IO ()
main = do
    let xs = 1 :| [2, 3]
    let ys = 4 :| [5, 6]
    putStrLn("*** " ++ show(prodNE3))