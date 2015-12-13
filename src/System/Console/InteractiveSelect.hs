module System.Console.InteractiveSelect (oneOf, manyOf) where

import Prelude
import System.IO
import System.Console.ANSI
import Data.Text.Format
import System.Console.Terminal.Size
import Data.Maybe (fromMaybe)
import System.Console.Types

oneOf :: Option option => [option] -> IO option
oneOf [row] = return row
oneOf rows = current <$> selectInteractively rows "Use j/k to move and Return to choose."


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
    hPrintOptionList handle Nothing options
    finalOptions <- askToChoose handle options

    hClose handle
    return finalOptions


askToChoose :: Option o => Handle -> OptionsList o -> IO (OptionsList o)
askToChoose handle options = do
    char <- hGetChar handle
    case char of
        'j'  -> recurse $ moveDown options
        'k'  -> recurse $ moveUp options
        ' '  -> recurse $ options { current = toggle (current options) }
        '\n' -> goToEnd >> return options
        _ -> askToChoose handle options
    where recurse nextOptions = hPrintOptionList handle (Just options) nextOptions >> askToChoose handle nextOptions
          goToEnd = hCursorMoveLinewise handle (length (below options) + 1)


moveDown :: Option option => OptionsList option -> OptionsList option
moveDown options@(OptionList _ _ _ [] []) = options
moveDown options@(OptionList _ above current (headBelow:restBelow) _) = options { above = above ++ [current]
                                                                                , current = headBelow
                                                                                , below = restBelow
                                                                                }


moveUp :: Option option => OptionsList option -> OptionsList option
moveUp options@(OptionList [] [] _ _ _) = options
moveUp options@(OptionList{..}) = options { above = init above
                                          , current = last above
                                          , below = current:below
                                          }


hPrintOptionList :: Option a
                 => Handle
                 -> Maybe (OptionsList a) -- Current version.
                 -> OptionsList a -- Next version.
                 -> IO ()
hPrintOptionList handle Nothing (OptionList _ above current below _) = do -- Initial render.
    mapM_ (hPrintOption handle False) above
    hPrintOption handle True current
    mapM_ (hPrintOption handle False) below
    hCursorUpLine handle $ length above + length below + 1
hPrintOptionList handle (Just currentOptions) nextOptions
    | currentOptions == nextOptions = return ()
    | otherwise = reprintOldCurrentLine >> hCursorMoveLinewise handle linesToMove >> reprintNewCurrentLine
    where linesToMove = length (above nextOptions) - length (above currentOptions)
          reprintOldCurrentLine = reprintCurrentLine currentOptions False
          reprintNewCurrentLine = reprintCurrentLine nextOptions True
          reprintCurrentLine options highlight = hPrintOption handle highlight (current options) >> hCursorUpLine handle 1


hPrintOption :: Option a => Handle -> Bool -> a -> IO ()
hPrintOption handle highlight option = do
    terminalWidth <- terminalDimension width handle
    hSetSGR handle [sgr highlight]
    hprint handle "{}\n" $ Only $ right terminalWidth ' ' (showOption option)
    hSetSGR handle [sgr False]


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
