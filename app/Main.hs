module Main where

import Lib

main :: IO ()
main = do
    chosen <- oneOf ["first", "second", "third", "sdflkjsdlfjksldkjf", "sssssssssssssssssssssssssssssssss", "oiuuuuuuuuuuuuuuuuuuuuuuuuuuwlkjs"]
    print chosen
