module Lib (oneOf) where

import Prelude
import System.IO
import System.Console.ANSI
import Data.Text.Format
import System.Console.Terminal.Size
import Data.Maybe (fromMaybe)

data Option a => Options a = Options { getAboveScreen :: [a]
                                     , getAboveCurrent :: [a] -- FIXME: have a VisibleOptions newtype after changing [] to Traversable.
                                     , getCurrent :: a
                                     , getBelowCurrent :: [a]
                                     , getBelowScreen :: [a]
                                     }

class Option a where
    showOption :: a -> String

instance Option String where
    showOption = id

oneOf :: Option options => [options] -> IO String
oneOf lines = do
    handle <- tty

    hSetBuffering handle NoBuffering
    hSetEcho handle False

    options <- buildOptions lines <$> getTerminalHeight handle
    printOptions Nothing options
    hCursorUpLine handle (length lines)

    char <- hGetChar handle
    case char of
        'j' -> moveDown 1 options

    hClose handle
    return.show $ char


moveDown :: Option option => Int -> Options option -> IO (Options option)
moveDown n options = undefined


buildOptions :: Option option => [option] -> Int -> Options option
buildOptions [] _ = undefined -- FIXME: handle this case properly.
buildOptions options@(headOption:tailOptions) terminalHeight =
    Options [] [] headOption (take belowLength tailOptions) (drop belowLength tailOptions)
    where belowLength | length options <= terminalHeight = length options - 1
                      | otherwise                        = terminalHeight - 1


tty :: IO Handle
tty = openFile "/dev/tty" ReadWriteMode


-- FIXME: Rewrite after adding VisibleOptions.
printOptions :: Option a
             => Maybe (Options a) -- Current version
             -> Options a -- Next version
             -> IO ()
printOptions Nothing options = do
    mapM_ (printLine False) (getAboveCurrent options)
    printLine True $ getCurrent options
    mapM_ (printLine False) (getBelowCurrent options)


printLine :: Option a => Bool -> a -> IO ()
printLine highlight option = do
    handle <- tty
    paddedString <- pad handle (showOption option)
    hSetSGR handle [sgr highlight]
    hprint handle "{}\n" $ Only paddedString


sgr :: Bool -> SGR
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
