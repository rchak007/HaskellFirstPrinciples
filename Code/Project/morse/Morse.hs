-- src/Morse.hs
module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , letterToMorse
    , morseToLetter
    ) where
import qualified Data.Map as M
type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
    ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

-- *Morse> letterToMorse 
-- fromList [('0',"-----"),('1',".----"),('2',"..---"),('3',"...--"),('4',"....-"),('5',"....."),('6',"-...."),('7',"--..."),('8',"---.."),('9',"----."),('a',".-"),('b',"-..."),('c',"-.-."),('d',"-.."),('e',"."),('f',"..-."),('g',"--."),('h',"...."),('i',".."),('j',".---"),('k',"-.-"),('l',".-.."),('m',"--"),('n',"-."),('o',"---"),('p',".--."),('q',"--.-"),('r',".-."),('s',"..."),('t',"-"),('u',"..-"),('v',"...-"),('w',".--"),('x',"-..-"),('y',"-.--"),('z',"--..")]

-- Small test for insert alone
testMapList :: (M.Map Char Morse)
testMapList = M.fromList [
    ('a', "1")
    , ('b', "2")
    ]
-- *Morse> :t M.insert
-- M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
-- *Morse> :M.insert 'c' "2" testMapList
-- fromList [('a',"1"),('b',"2"),('c',"2")]
-- *Morse> :t flip
-- flip :: (a -> b -> c) -> b -> a -> c
-- *Morse> flip M.insert "5" 'd' testMapList 
-- fromList [('a',"1"),('b',"2"),('d',"5")]


morseToLetter :: M.Map Morse Char
morseToLetter =
    M.foldrWithKey (flip M.insert) M.empty
        letterToMorse

-- we use foldrWithKey and flip insert to create the opposite list too. Morse to Char
-- *Morse> morseToLetter
-- fromList [("-",'t'),("--",'m'),("---",'o'),("-----",'0'),("----.",'9'),("---..",'8'),("--.",'g'),("--.-",'q'),("--..",'z'),("--...",'7'),("-.",'n'),("-.-",'k'),("-.--",'y'),("-.-.",'c'),("-..",'d'),("-..-",'x'),("-...",'b'),("-....",'6'),(".",'e'),(".-",'a'),(".--",'w'),(".---",'j'),(".----",'1'),(".--.",'p'),(".-.",'r'),(".-..",'l'),("..",'i'),("..-",'u'),("..---",'2'),("..-.",'f'),("...",'s'),("...-",'v'),("...--",'3'),("....",'h'),("....-",'4'),(".....",'5')]



charToMorse :: Char -> Maybe Morse
charToMorse c =
    M.lookup c letterToMorse
-- *Morse> charToMorse '0'
-- Just "-----"


stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
    sequence $ fmap charToMorse s
-- *Morse> stringToMorse "rafa"
-- Just [".-.",".-","..-.",".-"]
-- *Morse> fmap charToMorse "rafa"
-- [Just ".-.",Just ".-",Just "..-.",Just ".-"]
-- *Morse> sequence [Just ".-.",Just ".-",Just "..-.",Just ".-"]
-- Just [".-.",".-","..-.",".-"]



-- Prelude M> :t sequence
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

morseToChar :: Morse -> Maybe Char
morseToChar m =
    M.lookup m morseToLetter

-- Prelude M> :t M.lookup
-- M.lookup :: Ord k => k -> Map k a -> Maybe a

-- (base) Chakravartis-MacBook-Pro-2:morse chakravartiraghavan$ cabal exec which morse
-- /Users/chakravartiraghavan/Documents/Typora1/Blockchain/CardanoTraining/Haskell/Projects/morse/dist-newstyle/build/aarch64-osx/ghc-8.10.7/morse-0.1.0.0/x/morse/build/morse/morse
