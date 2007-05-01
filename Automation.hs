module Automation where


import Cards
import FreeCell
import Data.List
import Data.Maybe
import System.IO.Unsafe

dbg a = unsafePerformIO $ putStrLn (show a) >> return a

-- Move all unneeded cards to foundations.
foundateAllUnneeded :: Desk -> Desk
foundateAllUnneeded desk =
    let (more, desk') = foundateUnneeded desk
    in  if more then foundateAllUnneeded desk' else desk'

-- Move one unneeded card to fountaions.
-- Returns False if there was nothing to foundate.
foundateUnneeded :: Desk -> (Bool, Desk)
foundateUnneeded desk =
    let
        -- Environment.
        Desk stacks cells foundations = desk
        -- Predicates.
        liesOk (Card v1 s1, Card v2 s2) =  (v1 /= Ace)
                                        && (v2 == succ v1)
                                        && not (sameColor s1 s2)
        isAce (Card val _) = val == Ace
        isNeeded card = not (isAce card) &&
                        isJust (find (\c -> liesOk (c, card)) allCards)
        -- All cards at the desk
        allCards = concat stacks ++ allCells
        allCells = map (fromJust) $ filter (isJust) cells
        -- Execution functions are bound to possible.
        stack2found = map (stackToFoundation desk) [0..]
        cell2found = map (cellToFoundation desk) [0..]
        -- Bindings.
        numberedCells = map (\(a, b) -> (fromJust a, b)) $
                        filter (isJust.fst) $
                        zip cells cell2found
        numberedStacks = map (\(cards, executor) -> (head cards, executor)) $
                         filter (not.null.fst) $
                         zip stacks stack2found
        -- List of not needed cards.
        notNeeded = filter (not.isNeeded.fst) (numberedStacks ++ numberedCells)
        -- List of those, which can be moved to foundations.
        canFound = filter (isNothing.fst.snd) notNeeded
        -- Move a card to foundations.
        (err, desk') = snd $ head $ canFound
        more = not $ null $ tail canFound
        result | null canFound  = (False, desk)
               | otherwise      = (True, desk')
    in  result

-- Move the faced card from the given stack to a free cell.
stackToAnyCell :: Desk -> Int -> (Maybe String, Desk)
stackToAnyCell desk col =
    let
        Desk _ cells _ = desk
        lastCell = length cells - 1
        -- Find first free cell.
        possible = find (isNothing.fst) $ map (stackToCell desk col) $
            [0 .. lastCell]
    in  if isNothing possible then (Just "It isn't possible.", desk)
                              else fromJust possible

anyCellToStack :: Desk -> Int -> (Maybe String, Desk)
anyCellToStack desk col =
    let
        Desk _ cells _ = desk
        lastCell = length cells - 1
        possible = find (isNothing.fst) $ map (\i -> cellToStack desk i col) $
            [0 .. lastCell]
    in  if isNothing possible then (Just "It isn't possible.", desk)
                              else fromJust possible

anyCellToFoundation :: Desk -> (Maybe String, Desk)
anyCellToFoundation desk =
    let
        Desk _ cells _ = desk
        lastCell = length cells - 1
        possible = find (isNothing.fst) $ map (cellToFoundation desk) $
            [0 .. lastCell]
    in  if isNothing possible then (Just "It isn't possible.", desk)
                              else fromJust possible
