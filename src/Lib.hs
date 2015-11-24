module Lib (oneOf) where

import Prelude
import System.IO
import System.Console.ANSI
import Data.Text.Format
import System.Console.Terminal.Size
import Data.Maybe (fromMaybe)

type IsCurrent = Bool

data InteractivelySelectable a => Line a = Line a IsCurrent
data InteractivelySelectable a => Options a = Options { getAboveScreen :: [Line a]
                                                      , getAboveCurrent :: [Line a] -- FIXME: have a VisibleOptions newtype after changing [] to Traversable.
                                                      , getCurrent :: Line a
                                                      , getBelowCurrent :: [Line a]
                                                      , getBelowScreen :: [Line a]
                                                      }

class InteractivelySelectable a where
    showOption :: a -> String

instance InteractivelySelectable String where
    showOption = id

oneOf :: InteractivelySelectable options => [options] -> IO String
oneOf lines = do
    handle <- tty

    hSetBuffering handle NoBuffering
    hSetEcho handle False

    buildOptions lines <$> getTerminalHeight handle >>= printOptions
    hCursorUpLine handle (length lines)

    char <- hGetChar handle
    hClose handle
    return.show $ char


buildOptions :: (InteractivelySelectable option) => [option] -> Int -> Options option
buildOptions [] _ = undefined -- FIXME: handle this case properly.
buildOptions options@(headOption:tailOptions) terminalHeight =
    Options [] [] current (map toNotCurrentLine (take belowLength tailOptions)) (map toNotCurrentLine (drop belowLength tailOptions))
    where current = Line headOption True
          belowLength | length options <= terminalHeight = length options - 1
                      | otherwise                        = terminalHeight - 1
          toNotCurrentLine = (`Line` False)


tty :: IO Handle
tty = openFile "/dev/tty" ReadWriteMode


-- FIXME: Rewrite after adding VisibleOptions.
printOptions :: InteractivelySelectable a => Options a -> IO ()
printOptions options = do
    mapM_ printLine (getAboveCurrent options)
    printLine (getCurrent options)
    mapM_ printLine (getBelowCurrent options)


printLine :: InteractivelySelectable a => Line a -> IO ()
printLine (Line option isCurrent) = do
    handle <- tty
    paddedString <- pad handle (showOption option)
    hSetSGR handle [sgr isCurrent]
    hprint handle "{}\n" $ Only paddedString


sgr :: IsCurrent -> SGR
sgr True = SetSwapForegroundBackground True
sgr False = Reset

pad handle string = do
    terminalWidth <- getTerminalWidth handle
    return (right terminalWidth ' ' string)


getTerminalWidth :: Handle -> IO Int
getTerminalWidth handle = extractWidth <$> hSize handle
    where extractWidth = width . fromMaybe defaultTerminalWindow


getTerminalHeight :: Handle -> IO Int
getTerminalHeight handle = extractHeight <$> hSize handle
    where extractHeight = height . fromMaybe defaultTerminalWindow


defaultTerminalWindow = Window 24 80
