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
