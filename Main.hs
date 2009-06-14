module Main where

import System.Environment
import Data.List
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Directory

import Tracker.Api
import Tracker.Types
import Tracker.Config

main :: IO ()
main =
    getArgs >>= \args ->
    case getOpt RequireOrder options args of
      (opts, (cmd:rest), []) -> do cp <- loadCP (lookup "c" opts)
                                   runCmd cp cmd rest
      (_, _, errs) -> putStrLn (concat errs)

runCmd :: ConfigParser -> String -> [String] -> IO ()
runCmd _  "token"    [username, password] = putStrLn =<< token username password
runCmd cp "project"  _        = putProject =<< project (getToken cp) (getProject cp)
runCmd cp "projects" _        = putProjects =<< projects (getToken cp)
runCmd cp "stories"  _        = putStories =<< stories (getToken cp) (getProject cp)
runCmd cp "story"   [storyID] = putStory =<< story (getToken cp) (getProject cp) storyID
runCmd cp "search"  rest      = putStories =<< search (getToken cp) (getProject cp) (intercalate " " rest)
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


putItems :: (a -> IO ()) -> [a] -> IO ()
putItems putFunction items = mapM_ (\s -> putStrLn "" >> putFunction s) items

putItem :: [(a -> String, String)] -> a -> IO ()
putItem attrMap i = mapM_ (\(attr, l) -> putStrLn $ l ++ ": " ++ (attr i)) attrMap

putProjects :: [Project] -> IO ()
putProjects = putItems putProject

putProject :: Project -> IO ()
putProject = putItem [(prjName, "Name"),
                      (prjID, "ID"),
                      (prjIterationLength, "Iteration Length"),
                      (prjWeekStartDay, "Start Day"),
                      (prjPointScale, "Point Scale")]

putStories :: [Story] -> IO ()
putStories = putItems putStory

putStory :: Story -> IO ()
putStory = putItem [(stName         ,"Name"),
                    (stID           ,"ID"),
                    (stType         ,"Type"),
                    (stURL          ,"URL"),
                    (stEstimate     ,"Estimate"),
                    (stCurrentState ,"Status"),
                    (stRequestedBy  ,"Requestor"),
                    (stCreatedAt    ,"Created"),
                    (stLabels       ,"Labels"),
                    (stDescription  ,"Description")]

