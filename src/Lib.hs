module Lib (oneOf) where

import Prelude
import System.IO
import System.Console.ANSI
import Data.Text.Format
import System.Console.Terminal.Size
import Data.Maybe (fromMaybe)

type Current = Bool
data Line = Line String Current

oneOf :: [String] -> IO String
oneOf allLines@(firstLine:restOfLines) = do
    handle <- tty
    hSetBuffering handle NoBuffering
    hSetEcho handle False

    mapM_ printLine $ Line firstLine True : map (`Line` False) restOfLines
    hCursorUpLine handle (length allLines)

    char <- hGetChar handle
    return.show $ char


tty :: IO Handle
tty = openFile "/dev/tty" ReadWriteMode


printLine :: Line -> IO ()
printLine (Line string isCurrent) = do
    handle <- tty
    paddedString <- pad handle string
    hSetSGR handle [sgr isCurrent]
    hprint handle "{}\n" $ Only paddedString


sgr :: Current -> SGR
sgr True = SetSwapForegroundBackground True
sgr False = Reset

pad handle string = do
    terminalWidth <- getTerminalWidth handle
    return (right terminalWidth ' ' string)


getTerminalWidth :: Integral n => Handle -> IO n
getTerminalWidth handle = extractWidth <$> hSize handle
    where extractWidth = width . fromMaybe (Window 24 80)
