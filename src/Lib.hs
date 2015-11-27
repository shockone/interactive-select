module Lib (oneOf, manyOf) where

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

data Option o => TogglableOption o = TogglableOption o Bool deriving Eq

getOption :: Option o => TogglableOption o -> o
getOption (TogglableOption o _) = o

class Eq a => Option a where
    showOption :: a -> String
    toggle :: a -> a

instance Option String where
    showOption = id
    toggle = id

instance Option a => Option (TogglableOption a) where
    showOption (TogglableOption option False)= " [ ] " ++ showOption option
    showOption (TogglableOption option True)= " [x] " ++ showOption option
    toggle (TogglableOption o state) = TogglableOption o $ not state


oneOf :: Option option => [option] -> IO option
oneOf lines = do
    options <- selectInteractively lines "Use j/k to move and Return to choose."
    return (getCurrent options)


manyOf :: Option option => [option] -> IO [option]
manyOf lines = do
    options <- selectInteractively (map (`TogglableOption` False) lines) "Use j/k to move, Space to toggle and Return to choose."
    return (map getOption (filter isChosen (fromOptions options)))


selectInteractively :: Option o => [o] -> String -> IO (Options o)
selectInteractively o message = do
    handle <- tty
    hPrintHelpMessage handle message

    options <- toOptions o <$> getTerminalHeight handle
    hPrintOptions handle Nothing options

    finalOptions <- askToChoose handle options

    hClose handle

    return finalOptions

isChosen :: Option o => TogglableOption o -> Bool
isChosen (TogglableOption _ True) = True
isChosen (TogglableOption _ False) = False


askToChoose :: Option o => Handle -> Options o -> IO (Options o)
askToChoose handle options = do
    char <- hGetChar handle
    case char of
        'j' -> let nextOptions = moveDown options in hPrintOptions handle (Just options) nextOptions >> askToChoose handle nextOptions
        'k' -> let nextOptions = moveUp options in hPrintOptions handle (Just options) nextOptions >> askToChoose handle nextOptions
        ' ' -> let nextOptions = options { getCurrent = toggle (getCurrent options) } in hPrintOptions handle (Just options) nextOptions >> askToChoose handle nextOptions
        '\r' -> hCursorMoveLinewise handle (length (getBelowCurrent options) + 1) >> hSetSGR handle [sgr False] >> return options
        _ -> askToChoose handle options



moveDown, moveUp :: Option option => Options option -> Options option

moveDown options@(Options _ _ _ [] []) = options
moveDown options@(Options _ above current (headBelow:restBelow) _) = options { getAboveCurrent = above ++ [current]
                                                                             , getCurrent = headBelow
                                                                             , getBelowCurrent = restBelow
                                                                             }

moveUp options@(Options [] [] _ _ _) = options
moveUp options@(Options _ above current below _) = options { getAboveCurrent = init above
                                                           , getCurrent = last above
                                                           , getBelowCurrent = current:below
                                                           }


toOptions :: Option option => [option] -> Int -> Options option
toOptions [] _ = undefined -- FIXME: handle this case properly.
toOptions options@(headOption:tailOptions) terminalHeight =
    Options [] [] headOption (take belowLength tailOptions) (drop belowLength tailOptions)
    where belowLength | length options <= terminalHeight = length options - 1
                      | otherwise                        = terminalHeight - 1


fromOptions :: Option o => Options (TogglableOption o) -> [TogglableOption o]
fromOptions (Options aboveScreen above current below belowScreen) = aboveScreen ++ above ++ [current] ++ below ++ belowScreen

tty :: IO Handle
tty = do
    handle <- openFile "/dev/tty" ReadWriteMode

    hSetBuffering handle NoBuffering
    hSetEcho handle False

    return handle


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
    where pad handle string = do
            terminalWidth <- getTerminalWidth handle
            return (right terminalWidth ' ' string)


sgr :: Bool -> SGR
sgr True = SetSwapForegroundBackground True
sgr False = Reset



getTerminalWidth :: Handle -> IO Int
getTerminalWidth handle = extractWidth <$> hSize handle
    where extractWidth = width . fromMaybe defaultTerminalWindow


getTerminalHeight :: Handle -> IO Int
getTerminalHeight handle = extractHeight <$> hSize handle
    where extractHeight = height . fromMaybe defaultTerminalWindow


defaultTerminalWindow :: Window Int
defaultTerminalWindow = Window 24 80


hCursorMoveLinewise :: Handle -> Int -> IO ()
hCursorMoveLinewise handle n
    | n == 0 = return ()
    | n < 0 = hCursorUpLine handle (-n)
    | n > 0 = hCursorDownLine handle n
    | otherwise = hCursorDownLine handle n


hPrintHelpMessage :: Handle -> String -> IO ()
hPrintHelpMessage handle message = do
    hSetSGR handle [SetColor Foreground Dull Yellow]
    hPutStrLn handle message
    hSetSGR handle [Reset]
