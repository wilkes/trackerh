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
runCmd _  "token"    [username, password] = putStrLn    =<< token username password
runCmd cp "project"  [projectID]          = putProject  =<< project  (getToken cp) projectID
runCmd cp "projects" _                    = putProjects =<< projects (getToken cp)
runCmd cp "story"    [projectID, storyID] = putStory    =<< story    (getToken cp) projectID storyID
runCmd cp "stories"  [projectID]          = putStories  =<< stories  (getToken cp) projectID
runCmd cp "search"   (projectID:rest)     = putStories  =<< search   (getToken cp) projectID (intercalate " " rest)
runCmd cp "add"      (projectID:rest)     = putStory    =<< addStory (getToken cp) projectID (intercalate " " rest)
runCmd _  _          _                    = printUsage

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
                      \trackerh project PROJECT_ID\n\
                      \trackerh stories PROJECT_ID\n\
                      \trackerh story STORY_ID\n\
                      \trackerh add PROJECT_ID TITLE\n\
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

