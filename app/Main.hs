module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    chosen <- manyOf $ lines input
    print chosen
