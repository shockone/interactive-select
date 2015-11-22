module Main where

import Lib

main :: IO ()
main = do
    chosen <- oneOf ["first", "second", "third"]
    print chosen
