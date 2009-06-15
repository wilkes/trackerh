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
runCmd _  "token"     [uid, pwd]     = putStrLn    =<< token uid pwd
runCmd cp "project"   [pid]          = putProject  =<< project  (getToken cp) pid
runCmd cp "projects"  _              = putProjects =<< projects (getToken cp)
runCmd cp "story"     [pid, storyID] = putStory    =<< story    (getToken cp) pid storyID
runCmd cp "stories"   [pid]          = putStories  =<< stories  (getToken cp) pid
runCmd cp "search"    (pid:rest)     = putStories  =<< search   (getToken cp) pid (intercalate " " rest)
runCmd cp "add"       (pid:rest)     = putStory    =<< addStory (getToken cp) pid (intercalate " " rest)
runCmd cp "done"      [pid]          = putStories  =<< iterationGroup (getToken cp) pid "done"
runCmd cp "current"   [pid]          = putStories  =<< iterationGroup (getToken cp) pid "current"
runCmd cp "backlog"   [pid]          = putStories  =<< iterationGroup (getToken cp) pid "backlog"
runCmd _  _           _              = printUsage

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
                      \trackerh done PROJECT_ID\n\
                      \trackerh current PROJECT_ID\n\
                      \trackerh backlog PROJECT_ID\n\
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

