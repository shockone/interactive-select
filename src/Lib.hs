module Lib (oneOf, manyOf) where

import Prelude
import System.IO
import System.Console.ANSI
import Data.Text.Format
import System.Console.Terminal.Size
import Data.Maybe (fromMaybe)
import Types

oneOf :: Option option => [option] -> IO option
oneOf rows = getCurrent <$> selectInteractively rows "Use j/k to move and Return to choose."


manyOf :: Option option => [option] -> IO [option]
manyOf rows = do
    options <- selectInteractively (map (`TogglableOption` False) rows) "Use j/k to move, Space to toggle and Return to choose."
    return (map getOption (filter isChosen (fromOptionList options)))


tty :: IO Handle
tty = do
    handle <- openFile "/dev/tty" ReadWriteMode

    hSetBuffering handle NoBuffering
    hSetEcho handle False

    return handle


selectInteractively :: Option o => [o] -> String -> IO (OptionsList o)
selectInteractively rows message = do
    handle <- tty
    options <- toOptionList rows <$> terminalDimension height handle

    hPrintHelpMessage handle message
    hPrintOptions handle Nothing options
    finalOptions <- askToChoose handle options

    hClose handle
    return finalOptions


askToChoose :: Option o => Handle -> OptionsList o -> IO (OptionsList o)
askToChoose handle options = do
    char <- hGetChar handle
    case char of
        'j'  -> recurse $ moveDown options
        'k'  -> recurse $ moveUp options
        ' '  -> recurse $ options { getCurrent = toggle (getCurrent options) }
        '\n' -> goToEnd >> resetSGR >> return options
        _ -> askToChoose handle options
    where recurse nextOptions = hPrintOptions handle (Just options) nextOptions >> askToChoose handle nextOptions
          goToEnd = hCursorMoveLinewise handle (length (getBelowCurrent options) + 1)
          resetSGR = hSetSGR handle [sgr False]



moveDown :: Option option => OptionsList option -> OptionsList option
moveDown options@(OptionList _ _ _ [] []) = options
moveDown options@(OptionList _ above current (headBelow:restBelow) _) = options { getAboveCurrent = above ++ [current]
                                                                             , getCurrent = headBelow
                                                                             , getBelowCurrent = restBelow
                                                                             }


moveUp :: Option option => OptionsList option -> OptionsList option
moveUp options@(OptionList [] [] _ _ _) = options
moveUp options@(OptionList _ above current below _) = options { getAboveCurrent = init above
                                                              , getCurrent = last above
                                                              , getBelowCurrent = current:below
                                                              }


-- FIXME: Rewrite after adding VisibleOptions.
hPrintOptions :: Option a
             => Handle
             -> Maybe (OptionsList a) -- Current version
             -> OptionsList a -- Next version
             -> IO ()
hPrintOptions handle Nothing (OptionList _ above current below _) = do
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
    where pad handle string = do
            terminalWidth <- terminalDimension width handle
            return (right terminalWidth ' ' string)


sgr :: Bool -> SGR
sgr True = SetSwapForegroundBackground True
sgr False = Reset


terminalDimension :: (Window Int -> Int) -> Handle -> IO Int
terminalDimension dimensionGetter handle = extractDimension <$> hSize handle
    where extractDimension = dimensionGetter . fromMaybe defaultTerminalWindow
          defaultTerminalWindow = Window 24 80


hCursorMoveLinewise :: Handle -> Int -> IO ()
hCursorMoveLinewise handle n
    | n < 0 = hCursorUpLine handle (-n)
    | n > 0 = hCursorDownLine handle n
    | otherwise = return ()

hPrintHelpMessage :: Handle -> String -> IO ()
hPrintHelpMessage handle message = do
    hSetSGR handle [SetColor Foreground Dull Yellow]
    hPutStrLn handle message
    hSetSGR handle [Reset]
