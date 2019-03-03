-- | This module contains formatters and utilities to render Status to stdout.

module Hastatus.Outputs.Log
    ( logStatus
    , Color(..)
    , Style(..)
    , color
    , bgColor
    , style
    ) where

import Hastatus.Types
import Hastatus.Util

import Conduit

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

toPrettyStyle :: Style -> P.Style
toPrettyStyle c = case c of
    Normal        -> P.Normal
    Bold          -> P.Bold
    Faint         -> P.Faint
    Italic        -> P.Italic
    Underline     -> P.Underline
    SlowBlink     -> P.SlowBlink
    ColoredNormal -> P.ColoredNormal
    Reverse       -> P.Reverse

-- | Colorize Status
color :: Color -> Formatter
color c = mapC $ P.color $ toPrettyColor c

-- | Add background to Status
bgColor :: Color -> Formatter
bgColor c = mapC $ P.bgColor $ toPrettyColor c

-- | Add style to Status
style :: Style -> Formatter
style s = mapC $ P.style $ toPrettyStyle s
