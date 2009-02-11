module Main where

import System.Environment
import Tracker.Api
import Data.List

main = do args <- getArgs
          case args of
            ["token", username, password]        -> token username password
            ["projects", token]                  -> projects token
            ["project", token, pid]              -> project token pid
            ["stories", token, pid]              -> stories token pid
            ["story", token, pid, storyID]       -> story token pid storyID
            ("search" : token : pid : rest)      -> search token pid (intercalate " " rest)
            _ -> printUsage

printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects token\n\
                      \trackerh project token projectID\n\
                      \trackerh stories token projectID\n\
                      \trackerh story token projectID storyID\n\
                      \\n"


