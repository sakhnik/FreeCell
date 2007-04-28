module FreeCell where

import Cards
import Data.List
import Data.Maybe
import Test.QuickCheck

data Stacks = Stacks [[Card]]

-- Distribute elements of a list to columns:
-- [A, B, C, D, E, F, G, H, I, J, K, L, M] -> (4)
-- 1 | A B C D |
-- 2 | E F G H | -> [[A, E, I, M],[B,F,J],[C,G,K],[D,H,L]]
-- 3 | I J K L |
-- 4 | M       |
mkStacks :: Int -> [a] -> [[a]]
mkStacks columns pack =
    let num = [1 .. columns]
        -- There'll be `columns' stacks.
        nums = concat $ repeat num
        -- Number elements [1,..,columns,1,..,columns,1..].
        orderedPack = zip pack nums
        -- Filter elements with the order number i.
        stack i = filter ((==i) . snd) orderedPack
        -- Get only filtered elements without numbers.
        stack' i = map (fst) (stack i)
        -- Return list of desired stacks.
    in  map (stack') num

prop_mkStacks cols len =
    let a = [1 .. len]
        stacks = mkStacks cols a
        b = concat stacks
        sameLength = length a == length b
        sameElements = null $ a \\ b
        rightCols = cols == length stacks
    in  cols > 0 && len > 1 ==>
        sameLength && sameElements && rightCols

-- Types
type Stack = [Card]
type Cell = Maybe Card
type Foundation = Maybe Card
-- Game desk
data Desk = Desk [Stack] [Cell] [Foundation]

instance Show Desk where
    show = showDesk

{-
Desk prototype:
┌───┬───┬───┬───┬───────┬───┬───┬───┬───┐
│10♠│ . │ . │ . │   λ   │ . │ . │ . │ . │
├───┴───┴───┴───┘       └───┴───┴───┴───┤
├────┬────┬────┬────┬────┬────┬────┬────┤
│ A♣ │ 8♦ │ .  │ J♥ │ K♣ │ .  │ .  │ .  │
│ 2♦ │ .  │ .  │ .  │ .  │ .  │ .  │ .  │
└────┴────┴────┴────┴────┴────┴────┴────┘
-}
showDesk :: Desk -> String
showDesk (Desk stacks cells foundations) =
    let -- Empty cell or foundation.
        holder Nothing = " . │"
        -- A card on a cell or a foundation.
        holder (Just card) = show card ++ "│"
        -- Show cells and foundations.
        strHold = concat . map (holder)
        strRes = strHold cells
        strDest = strHold foundations

        -- Emty place in a stack.
        holder' Nothing = " .  │"
        -- A card on in a stack.
        holder' (Just card) = show card ++ " │"
        strStack = (++ "\n") . ("│" ++) . concat . map (holder')
        strStacks = concat $ map (strStack) (transpose' stacks)

    in  "┌───┬───┬───┬───┬───────┬───┬───┬───┬───┐\n"  ++
        "│" ++ strRes ++ "   λ   │" ++ strDest ++ "\n" ++
        "├─A─┴─B─┴─C─┴─D─┘       └─0─┴─0─┴─0─┴─0─┤\n"  ++
        "├────┬────┬────┬────┬────┬────┬────┬────┤\n"  ++
                         strStacks                     ++
        "└─1──┴─2──┴─3──┴─4──┴─5──┴─6──┴─7──┴─8──┘\n"

-- Special transposition, used to display stacks sibling-by-sibling.
transpose' :: [[a]] -> [[Maybe a]]
transpose' a =
    let
        -- Column count.
        cols = length a
        -- Row count.
        rows = maximum $ map (length) a
        -- Preparation.
        a'   = map (fill . map (Just) . reverse) a
        -- Padding with Nothing to common row count.
        fill = \l -> l ++ (replicate (rows - length l) Nothing)
    in  transpose a'

prop_transpose' l =
    let
        -- Hard coded test.
        -- |1 5 8|    |4 3 2 1|
        -- |2 6  | -> |7 6 5 -|
        -- |3 7  |    |8 - - -|
        -- |4    |
        a = [[1,2,3,4],[5,6,7],[8]]
        b = [[  Just 4,  Just 7 , Just 8 ],
             [  Just 3,  Just 6, Nothing ],
             [  Just 2,  Just 5, Nothing ],
             [  Just 1, Nothing, Nothing ]]
        hardOk = b == transpose' a
        -- Random data test.
        l' = transpose' l
        dimOk = length l == length (head l')
    in  not (null l) && not (null (head l)) ==>
            hardOk && dimOk

-- Deal new pack of cards.
deal :: [Card] -> Desk
deal pack =
    let cards = mkStacks 8 pack
        cells = replicate 4 Nothing
        foundations = replicate 4 Nothing
    in  Desk cards cells foundations

-- Returns the number of foundation.
tryFoundate :: [Foundation] -> Card -> Maybe Int
tryFoundate foundations (Card Ace _) =
    -- Ace can be placed only on an empty foundation.
    elemIndex Nothing foundations
tryFoundate foundations (Card Two suit) =
    -- Two can be place only on the ace of the same suit.
    let sameAce Nothing           = False
        sameAce (Just (Card v s)) = s == suit && v == Ace
    in  findIndex sameAce foundations
tryFoundate foundations (Card value suit) =
    -- Other cards must be placed subordinally.
    let samePred Nothing           = False
        samePred (Just (Card v s)) = s == suit && v == pred value
    in  findIndex samePred foundations


-- Move a card from the top of given stack to the foundation.
-- Returns an information string and new desk.
stackToFoundation :: Desk -> Int -> (Maybe String, Desk)
stackToFoundation desk@(Desk stacks cells foundations) col =
    let
        -- Desired stack.
        stack = stacks !! col
        -- Accessible card in the stack.
        card = head stack
        -- Maybe index in the foundations.
        ind = tryFoundate foundations card
        -- The rest of the stack.
        stack' = tail stack
        -- Stacks with modified stack.
        stacks' = take col stacks ++ [stack'] ++ drop (col + 1) stacks
        -- Index of the foundation.
        i = fromJust ind
        -- Updated foundations.
        foundations' = take i foundations
                    ++ [Just card]
                    ++ drop (i + 1) foundations
        desk' = Desk stacks' cells foundations'
        result | col < 0 || col >= length stacks
                                      = (Just "No such column.", desk)
               | null stack           = (Just "There's no card there.", desk)
               | isNothing ind        = (Just "Can't move the card there.",
                                         desk)
               | otherwise            = (Nothing, desk')
    in  result

-- Move a card from stacks to a free cell. If the requested cell
-- isn't free, another one is chosen automatically.
stackToCell :: Desk -> Int -> Int -> (Maybe String, Desk)
stackToCell desk col idx =
    let
        -- Extracting arguments.
        Desk stacks cells foundations = desk
        -- Is the requested cell free?
        cellIsFree = Nothing == cells !! idx
        -- Another free cell.
        freeCell = elemIndex Nothing cells
        -- Selected stack.
        stack = stacks !! col
        -- The rest of stack.
        stack' = tail stack
        -- Accessible card in the stack.
        card = head stack
        -- Modified cells.
        cells' i = take i cells ++ [Just card] ++ drop (i+1) cells
        -- An index of first free cell.
        idx' = fromJust freeCell
        -- Stacks with modified stack.
        stacks' = take col stacks ++ [stack'] ++ drop (col + 1) stacks
        -- The card has just been moved to the requested cell.
        desk1 = Desk stacks' (cells' idx) foundations
        -- The card has just been moved to another free cell.
        desk2 = Desk stacks' (cells' idx') foundations
        result | col < 0 || col >= length stacks
                                      = (Just "No such column.", desk)
               | null stack           = (Just "There's no card there.", desk)
               | isNothing freeCell   = (Just "There's no free cell.", desk)
               | idx < length cells && idx >= 0 && cellIsFree
                                      = (Nothing, desk1)
               | otherwise            = (Nothing, desk2)
    in  result

-- Move a card from the cell to stack.
cellToStack :: Desk -> Int -> Int -> (Maybe String, Desk)
cellToStack desk idx col =
    let
        -- Get arguments.
        Desk stacks cells foundations = desk
        -- The cell of interest.
        cell = cells !! idx
        -- Card on the cell.
        card = fromJust cell
        -- The stack of interest.
        stack = stacks !! col
        -- Modified cells.
        cells' = take idx cells ++ [Nothing] ++ drop (idx + 1) cells
        -- Modified stack.
        stack' = [card] ++ stack
        -- Modified stacks.
        stacks' = take col stacks ++ [stack'] ++ drop (col + 1) stacks
        -- Modified desk.
        desk' = Desk stacks' cells' foundations
        -- The topmost card to lie on.
        parent = head stack
        canMove = canLieUpon parent card
        -- The condition to do the move.
        canLieUpon (Card vp sp) (Card vc sc)
            =  not (sameColor sp sc)
            && vp == succ vc
        result | col < 0 || col >= length stacks
                                      = (Just "No such column.", desk)
               | idx < 0 || idx >= length cells
                                      = (Just "No such cell.", desk)
               | isNothing cell       = (Just "There's no card there.", desk)
               | null stack           = (Nothing, desk')
               | canMove              = (Nothing, desk')
               | otherwise            = (Just "Can't move the card there.",
                                         desk)
    in  result

-- Move a card from cells to foundation.
cellToFoundation :: Desk -> Int -> (Maybe String, Desk)
cellToFoundation desk idx =
    let
        -- Get the environment.
        Desk stacks cells foundations = desk
        -- The cell of interest.
        cell = cells !! idx
        -- The card on the cell.
        card = fromJust cell
        -- Check what foundation could we move that card to.
        fnd = tryFoundate foundations card
        -- Just index of the foundation.
        f = fromJust fnd
        -- Modified foundations list.
        foundations' =  take f foundations
                     ++ [Just card]
                     ++ drop (f + 1) foundations
        -- Modified free cells list.
        cells' = take idx cells ++ [Nothing] ++ drop (idx + 1) cells
        -- Modified desk.
        desk' = Desk stacks cells' foundations'
        result | idx < 0 || idx >= length cells
                                      = (Just "No such cell.", desk)
               | isNothing cell       = (Just "There's no card there.", desk)
               | isNothing fnd        = (Just "Can't do that.", desk)
               | otherwise            = (Nothing, desk')
    in  result

stackToStack :: Desk -> Int -> Int -> (Maybe String, Desk)
stackToStack desk col1 col2 =
    let
        -- Environment.
        Desk stacks cells foundations = desk
        stack1 = stacks !! col1
        stack2 = stacks !! col2
        -- Count of free cells and piles.
        freeCells = foldl (\i c -> if isJust c then i + 1 else i) 0 cells
        freePiles = foldl (\i p -> if null p then i + 1 else i) 0 stacks
        -- Check if c1 can be lied on c2.
        liesOk (Card v1 s1, Card v2 s2) =  (v1 /= Ace)
                                        && (v2 == succ v1)
                                        && not (sameColor s1 s2)
        -- Length of maximal sequence in the stack1.
        maxSeq = (+1) $ length $ takeWhile liesOk $ zip stack1 (drop 1 stack1)
        -- To empty.
        -- Maximum number of cards that could be moved.
        permit_1 = (freeCells + 1) * freePiles
        -- Actual number of moved cards.
        count_1 = minimum [maxSeq, permit_1]
        -- Adjust environment.
        stack1_1 = drop count_1 stack1
        stack2_1 = take count_1 stack1
        stacks_1' = take col1 stacks ++ [stack1_1] ++ drop (col1 + 1) stacks
        stacks_1'' =  take col2 stacks_1'
                   ++ [stack2_1]
                   ++ drop (col2 + 1) stacks_1'
        desk_1 = Desk stacks_1'' cells foundations
        -- To non-empty stack.
        -- The card which will receive the part of stack1.
        card = head stack2
        permit_2 = (freeCells + 1) * (freePiles + 1)
        c_2 = minimum [maxSeq, permit_2]
        common = take c_2 stack1
        -- Count of cards to move.
        splitPoint = findIndex (\c -> liesOk (c, card)) common
        count_2 = 1 + fromJust splitPoint
        -- Adjust the environment.
        stack1_2 = drop count_2 stack1
        stack2_2 = (take count_2 stack1) ++ stack2
        stacks_2' = take col1 stacks ++ [stack1_2] ++ drop (col1 + 1) stacks
        stacks_2'' =  take col2 stacks_2'
                   ++ [stack2_2]
                   ++ drop (col2 + 1) stacks_2'
        desk_2 = Desk stacks_2'' cells foundations
        --
        result | col1 < 0 || col1 >= length stacks ||
                 col2 < 0 || col2 >= length stacks
                                      = (Just "No such pile.", desk)
               | col1 == col2         = (Just "You mustn't do that.", desk)
               | null stack1          = (Just "Nothing to move.", desk)
               | null stack2          = (Nothing, desk_1)
               | isNothing splitPoint = (Just "Can't do that.", desk)
               | otherwise            = (Nothing, desk_2)
    in  result
