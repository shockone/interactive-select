module Types where

data Option a => OptionsList a = OptionList { getAboveScreen :: [a]
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
    showOption (TogglableOption option state) = checkBox state ++ showOption option
        where checkBox True = " [x] "
              checkBox False = " [ ] "
    toggle (TogglableOption o state) = TogglableOption o $ not state

