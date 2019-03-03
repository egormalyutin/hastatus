-- | This module contains formatters and utilities to render Status to stdout.

module Hastatus.Outputs.Log

import qualified System.Console.Pretty as P

-- | Render Status to stdout
logStatus :: Status a -> IO ()
logStatus s = do
    let stat = noopConduit =$$ s
    outputs <- getOutputs stat
    mapM_ (\o -> runConduit $ o .| mapM_C putStrLn) outputs

-- | Colors for formatters
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Show, Read)

toPrettyColor :: Color -> P.Color
toPrettyColor c = case c of
    Black   -> P.Black
    Red     -> P.Red
    Green   -> P.Green
    Yellow  -> P.Yellow
    Blue    -> P.Blue
    Magenta -> P.Magenta
    Cyan    -> P.Cyan
    White   -> P.White

-- | Styles for formatters
data Style = Normal | Bold | Faint | Italic | Underline | SlowBlink | ColoredNormal | Reverse deriving (Show, Read)
