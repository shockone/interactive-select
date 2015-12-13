module System.Console.Types where

data Option a => OptionsList a = OptionList { aboveScreen :: [a]
                                            , above :: [a]
                                            , current :: a
                                            , below :: [a]
                                            , belowScreen :: [a]
                                            } deriving Eq

data Option o => TogglableOption o = TogglableOption { getOption :: o
                                                     , getState :: Bool
                                                     } deriving Eq

class Eq a => Option a where
    showOption :: a -> String
    toggle :: a -> a

instance Option String where
    showOption = id
    toggle = id

instance Option a => Option (TogglableOption a) where
    showOption togglable = checkBox (getState togglable) ++ showOption (getOption togglable)
        where checkBox True  = " [x] "
              checkBox False = " [ ] "
    toggle option = option { getState = not (getState option) }


isChosen :: Option o => TogglableOption o -> Bool
isChosen (TogglableOption _ True) = True
isChosen (TogglableOption _ False) = False


toOptionList :: Option option => [option] -> Int -> OptionsList option
toOptionList [] _ = undefined -- FIXME: handle this case properly.
toOptionList options@(headOption:tailOptions) terminalHeight =
    OptionList [] [] headOption (take belowLength tailOptions) (drop belowLength tailOptions)
    where belowLength | length options <= terminalHeight = length options - 1
                      | otherwise                        = terminalHeight - 1


fromOptionList :: Option o => OptionsList (TogglableOption o) -> [TogglableOption o]
fromOptionList (OptionList aboveScreen above current below belowScreen) = aboveScreen ++ above ++ [current] ++ below ++ belowScreen
