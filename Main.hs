module Main where

import System.Console.GetOpt
import System.Environment

import qualified TextUI as TUI

data Flag = Help
          | Text TUI.Config

options :: [OptDescr Flag]
options =
    [ Option ['?','h'] ["help"] (NoArg Help)
        "Help message."
    , Option ['t']     ["text"] (OptArg textConfig "uc")
        "Use text user interface with options: u(nicode), c(olors)."
    ]

textConfig :: Maybe String -> Flag
textConfig Nothing = Text $ TUI.Config { TUI.colors  = False,
                                         TUI.unicode = False }
textConfig (Just opts) =
    let
        unicode = elem 'u' opts
        colors  = elem 'c' opts
    in  Text $ TUI.Config { TUI.colors = colors, TUI.unicode = unicode }

doOptions :: [Flag] -> IO ()
doOptions [] = TUI.newGame $ TUI.Config { TUI.colors = False,
                                          TUI.unicode = False }
doOptions [Help] = putStrLn $ usage
doOptions [Text config] = TUI.newGame config
doOptions _ = do
    putStrLn $ "There must be exactly one option specified."
    putStrLn $ usage

usage :: String
usage = usageInfo "Usage: FreeCell [options]" options

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (o, [], [])  -> doOptions o
        (_, n, [])   -> do
            putStrLn $ concat $ map ("Unknown option: " ++) n
            putStrLn $ usage
        (_, _, errs) -> do
            putStrLn $ concat errs ++ usage
    --let config = TUI.Config { TUI.colors = True, TUI.unicode = True }
    --TextUI.newGame config
