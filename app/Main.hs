module Main where

import Lib

main :: IO ()
main = getContents >>= manyOf . lines >>= mapM_ putStrLn
