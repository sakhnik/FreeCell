module Cards(
    -- Data types
    Suit (Spades, Clubs, Diamonds, Hearts),
    Value (Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
        Jack, Queen, King, Ace),
    Card (Card),
    SuitColor (Black, Red),
    -- functions
    suitColor,
    sameColor,
    pack36,
    pack52,
    shuffle
    ) where

import System.Random
import Data.List
import Test.QuickCheck

data Suit = Spades
          | Clubs
          | Diamonds
          | Hearts
          deriving Eq

instance Show Suit where
    show = showSuit

instance Arbitrary Suit where
    coarbitrary = undefined
    arbitrary = oneof $ map (return) [Spades, Clubs, Diamonds, Hearts]

showSuit :: Suit -> String
showSuit Spades   = "\x1b[34m♠\x1b[0m"
showSuit Clubs    = "\x1b[34m♣\x1b[0m"
showSuit Diamonds = "\x1b[31m♦\x1b[0m"
showSuit Hearts   = "\x1b[31m♥\x1b[0m"

data SuitColor = Black
               | Red
               deriving (Eq, Show)

suitColor :: Suit -> SuitColor
suitColor Spades = Black
suitColor Clubs  = Black
suitColor _      = Red

sameColor :: Suit -> Suit -> Bool
sameColor a b = (suitColor a) == (suitColor b)

data Value = Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
           | Ace
           deriving (Eq, Enum)

instance Show Value where
    show = showValue

instance Arbitrary Value where
    coarbitrary = undefined
    arbitrary = oneof $ map (return) [Two, Three, Four, Five, Six, Seven,
        Eight, Nine, Ten, Jack, Queen, King, Ace]

showValue :: Value -> String
showValue Two   = " 2"
showValue Three = " 3"
showValue Four  = " 4"
showValue Five  = " 5"
showValue Six   = " 6"
showValue Seven = " 7"
showValue Eight = " 8"
showValue Nine  = " 9"
showValue Ten   = "10"
showValue Jack  = " J"
showValue Queen = " Q"
showValue King  = " K"
showValue Ace   = " A"

data Card = Card Value Suit
          deriving Eq

instance Show Card where
    show = showCard

showCard :: Card -> String
showCard (Card value suit) =
    let color = case suitColor suit of
                    Red   -> "\x1b[31m"
                    Black -> "\x1b[34m"
        noColor = "\x1b[0m"
    in color ++ show value ++ noColor ++ show suit

-- Carthesian product of two lists
cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct a b =
    let a' = concat $ map (replicate (length b)) a
        b' = concat $ replicate (length a) b
    in  zip a' b'

prop_cartProduct len1 len2 =
    let a = [1 .. len1]
        b = [1 .. len2]
        prod = cartProduct a b
        sameLength = length prod == (length a) * (length b)
        onlyOnce = length prod == length (nub prod)
        known = and $ map (\(x, y) -> (elem x a) && (elem y b)) prod
    in  len1 > 1 && len2 > 1 ==>
        sameLength && onlyOnce && known

-- Make a pack of cards.
mkPack :: [Value] -> [Suit] -> [Card]
mkPack values suits =
    let vs = cartProduct values suits
    in  map (\(v, s) -> Card v s) vs

-- The standard pack of 36 cards.
pack36 :: [Card]
pack36 =
    let values = [Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
        suits  = [Spades, Clubs, Diamonds, Hearts]
    in mkPack values suits

-- The standard pack of 52 cards.
pack52 :: [Card]
pack52 =
    let values = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
                  Jack, Queen, King, Ace]
        suits  = [Spades, Clubs, Diamonds, Hearts]
    in mkPack values suits

-- Shuffle given pack of cards, using random seed.
shuffle :: Int -> [a] -> [a]
shuffle seed pack =
    let stdGen = mkStdGen seed
        -- A list of random indices.
        rndIdx 0   _   = []
        rndIdx len gen = [a] ++ (rndIdx (len - 1) gen')
            where
                (a, gen') = randomR (0, len - 1) gen
        -- Extract of random cards from the pack pk.
        result [] _   = []
        result pk idx = [card] ++ result pk' (tail idx)
            where
                i    = head idx                     -- current index
                card = pk !! i                      -- chosen random card
                pk'  = take i pk ++ drop (i+1) pk   -- pack without chosen card
        rndIdx' = rndIdx (length pack) stdGen
    in  result pack rndIdx'

prop_shuffle i l =
    let b = shuffle i a
        a = [1 .. l]
        sameLength = length a == length b
        sameElements = null $ b \\ a
        changed = or $ zipWith (/=) a b
    in  l > 1 ==>
        sameLength && sameElements && changed
