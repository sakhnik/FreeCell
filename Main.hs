module Main where

import IO
import System.Random
import Data.Char

import qualified Cards
import qualified FreeCell
import qualified Automation
import qualified History
import qualified UTF8

-- Past, present, future.
data Game = Game ([FreeCell.Desk], FreeCell.Desk, [FreeCell.Desk])


toUtf8 :: String -> String
toUtf8 s = map (chr . fromEnum) (UTF8.encode s)

putStrUtf8Ln :: String -> IO ()
putStrUtf8Ln = putStrLn . toUtf8

printHelp :: IO ()
printHelp = do
    putStrLn "Haskell implementation of free cell patience."
    putStrLn ""
    putStrLn "q\t\t\tExit"
    putStrLn "h,?\t\t\tHelp message"
    putStrLn "n\t\t\tNew game"
    putStrLn "u\t\t\tUndo"
    putStrLn "y\t\t\tRedo"
    putStrLn "r\t\t\tRestart from the beginning."
    putStrLn "<src><dst>\t\tMove a card from <src> to the <dst>"
    putStrLn ""
    putStrLn "Good luck, and keep patience!"

doMove :: Char -> Char -> FreeCell.Desk -> (Maybe String, FreeCell.Desk)
doMove from to desk =
    let
        f = digitToInt from
        t = digitToInt to
        -- Check what a coordinate number mean.
        isStack idx = 0 < idx && idx < 9
        isCell idx = 9 <= idx && idx < 14
        isFnd idx = idx == 0
        -- Translate coordinate to the index.
        cellId idx = if idx > 9 then idx - 10 else 0
        stackId idx = idx - 1
        -- Game rules.
        stack2found = FreeCell.stackToFoundation desk
        stack2cell = FreeCell.stackToCell desk
        cell2stack = FreeCell.cellToStack desk
        cell2found = FreeCell.cellToFoundation desk
        stack2stack = FreeCell.stackToStack desk
        result | isStack f && isFnd t     = stack2found (stackId f)
               | isStack f && isCell t    = stack2cell (stackId f) (cellId t)
               | isCell f && isStack t    = cell2stack (cellId f) (stackId t)
               | isCell f && isFnd t      = cell2found (cellId f)
               | isStack f && isStack t   = stack2stack (stackId f) (stackId t)
               | otherwise                = (Just "Don't know how to do it.",
                                             desk)
    in  result

doGame :: Game -> IO ()
doGame game = do
    putStrLn ""
    let desk = Automation.foundateAllUnneeded desk1
        desk1 = History.present g
        Game g = game
    putStrUtf8Ln $ show $ desk
    putStr $ "Your move: "
    hFlush stdout
    move <- getLine
    case move of
        'q':_        -> return ()
        'h':_        -> printHelp >> doGame game
        '?':_        -> printHelp >> doGame game
        'n':_        -> newGame
        'u':_        -> do
            let hasPast = History.hasPrevious g
                g' = History.previous g
                game' = Game g'
            if hasPast
                then doGame game'
                else do
                    putStrLn "Sorry, this is the last situation, I remember."
                    doGame game
        'y':_        -> do
            let hasFuture = History.hasNext g
                g' = History.next g
                game' = Game g'
            if hasFuture
                then doGame game'
                else do
                    putStrLn "Sorry, this is the newest situation, I know."
                    doGame game
        'r':_        -> do
            let g' = History.oldest g
                game' = Game g'
            doGame game'
        from:to:rest -> do
            let
                inputOk = isHexDigit from && isHexDigit to
                (err, desk') | inputOk   = doMove from to desk
                             | otherwise = (Just "Must be hex digits.", desk)
            case err of
                Just strErr -> putStrLn strErr >> doGame game
                Nothing     -> do
                    let g' = History.record g desk'
                        game' = Game g'
                    doGame game'
        _            -> putStrLn "Hugh?" >> doGame game

newGame :: IO ()
newGame = do
    let pack = Cards.pack52
    seed <- randomIO
    let shuffled = Cards.shuffle seed pack
        desk = FreeCell.deal shuffled
        game = Game $ History.new desk
    doGame game

main :: IO ()
main = newGame
