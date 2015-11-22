module Lib (oneOf) where

oneOf :: Show x => [x] -> IO x
oneOf xs = do
    mapM_ putStrLn xs
    return $ head xs
