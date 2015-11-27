module Main where

import Lib

main :: IO ()
main = do
    chosen <- manyOf ["first", "second", "third", "sdflkjsdlfjksldkjf", "sssssssssssssssssssssssssssssssss", "oiuuuuuuuuuuuuuuuuuuuuuuuuuuwlkjs"]
    print chosen
