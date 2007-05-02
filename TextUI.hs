module TextUI where

import IO
import System.Random
import Data.Char

import qualified Cards as C
import qualified FreeCell as FC
import qualified Automation
import qualified History
import qualified UTF8

data Config = Config { colors::Bool, unicode::Bool }

-- Past, present, future.
type Game = ([FC.Desk], FC.Desk, [FC.Desk])


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

doMove :: Char -> Char -> FC.Desk -> (Maybe String, FC.Desk)
doMove from to desk =
    let
        f = digitToInt from
        t = digitToInt to
        -- Check what a coordinate number mean.
        isStack idx = 0 < idx && idx < 9
        isCell idx = 9 < idx && idx < 14
        isFnd idx = idx == 0
        isAnyCell idx = idx == 9
        -- Translate coordinate to the index.
        cellId idx = idx - 10
        stackId idx = idx - 1
        -- Game rules.
        stack2found = FC.stackToFoundation desk
        stack2cell = FC.stackToCell desk
        cell2stack = FC.cellToStack desk
        cell2found = FC.cellToFoundation desk
        stack2stack = FC.stackToStack desk
        stack2anyCell = Automation.stackToAnyCell desk
        anyCell2stack = Automation.anyCellToStack desk
        anyCell2found = Automation.anyCellToFoundation desk
        result | isStack f && isFnd t     = stack2found (stackId f)
               | isStack f && isCell t    = stack2cell (stackId f) (cellId t)
               | isStack f && isAnyCell t = stack2anyCell (stackId f)
               | isCell f && isStack t    = cell2stack (cellId f) (stackId t)
               | isAnyCell f && isStack t = anyCell2stack (stackId t)
               | isCell f && isFnd t      = cell2found (cellId f)
               | isAnyCell f && isFnd t   = anyCell2found
               | isStack f && isStack t   = stack2stack (stackId f) (stackId t)
               | otherwise                = (Just "Don't know how to do it.",
                                             desk)
    in  result

printCard :: Config -> C.Card -> String
printCard config (C.Card face suit) =
    let
        unicodeSuit C.Spades   = "♠"
        unicodeSuit C.Clubs    = "♣"
        unicodeSuit C.Diamonds = "♦"
        unicodeSuit C.Hearts   = "♥"
        sSuit = if unicode config then unicodeSuit suit else show suit
        sFace = show face
        sColor C.Black = "\x1b[34m"
        sColor C.Red   = "\x1b[31m"
        sNoColor       = "\x1b[0m"
        color = C.suitColor suit
        sCard = if colors config
            then sColor color ++ sFace ++ sSuit ++ sNoColor
            else sFace ++ sSuit
    in  sCard

printDesk :: Config -> FC.Desk -> IO ()
printDesk config desk = do
    putStrUtf8Ln $ FC.showDesk desk (printCard config)

doGame :: Config -> Game -> IO ()
doGame config game = do
    putStrLn ""
    let desk = Automation.foundateAllUnneeded desk1
        desk1 = History.present game
    printDesk config desk
    putStr $ "Your move: "
    hFlush stdout
    move <- getLine
    case move of
        'q':_        -> return ()
        'h':_        -> printHelp >> doGame config game
        '?':_        -> printHelp >> doGame config game
        'n':_        -> newGame config
        'u':_        -> do
            let hasPast = History.hasPrevious game
                game' = History.previous game
            if hasPast
                then doGame config game'
                else do
                    putStrLn "Sorry, this is the last situation, I remember."
                    doGame config game
        'y':_ -> do
            let hasFuture = History.hasNext game
                game' = History.next game
            if hasFuture
                then doGame config game'
                else do
                    putStrLn "Sorry, this is the newest situation, I know."
                    doGame config game
        'r':_ -> do
            let game' = History.oldest game
            doGame config game'
        from:to:rest -> do
            let
                inputOk = isHexDigit from && isHexDigit to
                (err, desk') | inputOk   = doMove from to desk
                             | otherwise = (Just "Must be hex digits.", desk)
            case err of
                Just strErr -> putStrLn strErr >> doGame config game
                Nothing     -> do
                    let game' = History.record game desk'
                    doGame config game'
        _ -> putStrLn "Hugh?" >> doGame config game

newGame :: Config -> IO ()
newGame config = do
    let pack = C.pack52
    seed <- randomIO
    let shuffled = C.shuffle seed pack
        desk = FC.deal shuffled
        game = History.new desk
    doGame config game

