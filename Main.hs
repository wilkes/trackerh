module Main where

import System.Environment
import Data.List
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Directory

import Tracker.Api
import Tracker.Config

main :: IO ()
main =
    getArgs >>= \args ->
    case getOpt RequireOrder options args of
      (opts, (cmd:rest), []) -> do cp <- loadCP (lookup "c" opts)
                                   runCmd cp cmd rest
      (_, _, errs) -> putStrLn (concat errs)

runCmd :: ConfigParser -> String -> [String] -> IO ()
runCmd _  "token"    [username, password] = token username password
runCmd cp "projects" _        = projects (getToken cp)
runCmd cp "project"  _        = project (getToken cp) (getProject cp)
runCmd cp "stories"  _        = stories (getToken cp) (getProject cp)
runCmd cp "story"   [storyID] = story (getToken cp) (getProject cp) storyID
runCmd cp "search"  rest      = search (getToken cp) (getProject cp) (intercalate " " rest)
runCmd _ _ _ = printUsage

loadCP :: Maybe FilePath -> IO ConfigParser
loadCP Nothing   = getUserDocumentsDirectory >>= \userDir ->
                   loadCP $ Just $ userDir ++ "/.trackerh"
loadCP (Just fp) = loadConfig fp

options :: [OptDescr StdOption]
options = [Option "c" ["config"] (ReqArg (stdRequired "c") "FILE") "Specify config"]

printUsage :: IO ()
printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects\n\
                      \trackerh project\n\
                      \trackerh stories\n\
                      \trackerh story storyID\n\
                      \\n"


