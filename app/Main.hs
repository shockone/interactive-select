module Main where

import Options.Applicative
import System.Console.InteractiveSelect

data Mode = One | Many

main :: IO ()
main = do
    mode <- execParser parserInfo
    fmap lines getContents >>= select mode


parserInfo :: ParserInfo Mode
parserInfo = info (helper <*> parser) description
    where parser      = flag One Many $ long "many" <> short 'm' <> help "Select multiple options."
          description = fullDesc <> progDesc "Interactively select from whatever is passed to STDIN"


select :: Mode -> [String] -> IO ()
select One rows = oneOf rows >>= putStrLn
select Many rows = manyOf rows >>= mapM_ putStrLn
