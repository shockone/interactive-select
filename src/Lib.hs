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
                                     } deriving Eq

class Eq a => Option a where
    showOption :: a -> String

instance Option String where
    showOption = id

oneOf :: Option options => [options] -> IO String
oneOf lines = do
    handle <- tty

    hSetBuffering handle NoBuffering
    hSetEcho handle False

    options <- buildOptions lines <$> getTerminalHeight handle
    hPrintOptions handle Nothing options

    listenToKeyboard handle options

    hClose handle
    return "The End."


listenToKeyboard :: Option o => Handle -> Options o -> IO ()
listenToKeyboard handle options = do
    char <- hGetChar handle
    case char of
        'j' -> let nextOptions = moveDown options in hPrintOptions handle (Just options) nextOptions >> listenToKeyboard handle nextOptions
        'k' -> let nextOptions = moveUp options in hPrintOptions handle (Just options) nextOptions >> listenToKeyboard handle nextOptions
        _ -> return ()



moveDown, moveUp :: Option option => Options option -> Options option

moveDown options@(Options _ above current (headBelow:restBelow) _) = options { getAboveCurrent = above ++ [current]
                                                                             , getCurrent = headBelow
                                                                             , getBelowCurrent = restBelow
                                                                             }

moveUp options@(Options _ above current below _) = options { getAboveCurrent = init above
                                                           , getCurrent = last above
                                                           , getBelowCurrent = current:below
                                                           }


buildOptions :: Option option => [option] -> Int -> Options option
buildOptions [] _ = undefined -- FIXME: handle this case properly.
buildOptions options@(headOption:tailOptions) terminalHeight =
    Options [] [] headOption (take belowLength tailOptions) (drop belowLength tailOptions)
    where belowLength | length options <= terminalHeight = length options - 1
                      | otherwise                        = terminalHeight - 1


tty :: IO Handle
tty = openFile "/dev/tty" ReadWriteMode


-- FIXME: Rewrite after adding VisibleOptions.
hPrintOptions :: Option a
             => Handle
             -> Maybe (Options a) -- Current version
             -> Options a -- Next version
             -> IO ()
hPrintOptions handle Nothing (Options _ above current below _) = do
    mapM_ (hPrintLine handle False) above
    hPrintLine handle True current
    mapM_ (hPrintLine handle False) below
    hCursorUpLine handle $ length above + length below + 1
hPrintOptions handle (Just currentOptions) nextOptions
    | currentOptions == nextOptions = return ()
    | otherwise = do
        hPrintLine handle False (getCurrent currentOptions)
        hCursorUpLine handle 1
        hCursorMoveLinewise handle linesToMove
        hPrintLine handle True (getCurrent nextOptions)
        hCursorUpLine handle 1
    where linesToMove = length (getAboveCurrent nextOptions) - length (getAboveCurrent currentOptions)

hPrintLine :: Option a => Handle -> Bool -> a -> IO ()
hPrintLine handle highlight option = do
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


hCursorMoveLinewise :: Handle -> Int -> IO ()
hCursorMoveLinewise handle n
    | n == 0 = return ()
    | n < 0 = hCursorUpLine handle (-n)
    | n > 0 = hCursorDownLine handle n
    | otherwise = hCursorDownLine handle n
