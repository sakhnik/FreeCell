module History where

-- Create new history.
new :: a -> ([a], a, [a])
new a = ([], a, [])

-- Get present value.
present :: ([a], a, [a]) -> a
present (_, present, _) = present

-- Accept new present value. Adjust history.
record :: ([a], a, [a]) -> a -> ([a], a, [a])
record (past, prev, _) cur = ([prev] ++ past, cur, [])

-- Check if we have the past.
hasPrevious :: ([a], a, [a]) -> Bool
hasPrevious ([], _, _) = False
hasPrevious _          = True

-- Move back in the past.
previous :: ([a], a, [a]) -> ([a], a, [a])
previous (past, present, future) =
    let
        past'    = tail past
        present' = head past
        future'  = [present] ++ future
    in (past', present', future')

-- Check if we have the future.
hasNext :: ([a], a, [a]) -> Bool
hasNext (_, _, []) = False
hasNext _          = True

-- Move forward to the future.
next :: ([a], a, [a]) -> ([a], a, [a])
next (past, present, future) =
    let
        past'    = [present] ++ past
        present' = head future
        future'  = tail future
    in  (past', present', future')

-- Move to the oldest value.
oldest :: ([a], a, [a]) -> ([a], a, [a])
oldest time@([], _, _) = time
oldest time@_ = oldest $ previous time
