module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Directory
import Data.ConfigFile
import Data.Either.Utils(forceEither)
import Control.Applicative((<$>))
import Tracker.Api
import Tracker.Context

main :: IO ()
main = do 
    args <- getArgs
    case getOpt RequireOrder o args of
      (opts, (cmd:rest), []) -> do cp <- loadCP (lookup "c" opts)
                                   runCmd cp cmd rest
      (_, _, errs) -> putStrLn (concat errs)
    where
      o = [Option "c" ["config"] (ReqArg (stdRequired "c") "FILE") "Specify config"]

runCmd :: ConfigParser -> String -> [String] -> IO ()
runCmd _  "token"     [uid, pwd]     = putStrLn =<< getToken uid pwd
runCmd cp "projects"  _              = runP getProjects                             putProjects   (cpToken cp) ""
runCmd cp "activities"  []           = runP getActivities                           putActivities (cpToken cp) ""
runCmd cp "activities"  [pid]        = runP getActivities                           putActivities (cpToken cp) pid
runCmd cp "project"   [pid]          = runP getProject                              putProject    (cpToken cp) pid
runCmd cp "stories"   [pid]          = runP (getStories 0 0)                        putStories    (cpToken cp) pid 
runCmd cp "stories"   [pid,l,o]      = runP (getStories (read l) (read o))          putStories    (cpToken cp) pid 
runCmd cp "search"    (pid:rest)     = runP (search (intercalate " " rest))         putStories    (cpToken cp) pid 
runCmd cp "mywork"    [pid,user]     = runP (filterStories (MyWork user))           putStories    (cpToken cp) pid
runCmd cp "story"     [pid, sid]     = runP (getStory sid)                          putStory      (cpToken cp) pid 
runCmd cp "delete"    [pid, sid]     = runP (deleteStory sid)                       putStory      (cpToken cp) pid 
runCmd cp "add"       (pid:rest)     = runP (addStory (intercalate " " rest))       putStory      (cpToken cp) pid 
runCmd cp "comment"   (pid:sid:rest) = runP (addComment sid (intercalate " " rest)) putNote       (cpToken cp) pid
runCmd cp "deliver"   [pid]          = runP deliverAllFinished                      putStories    (cpToken cp) pid
runCmd cp "done"      [pid]          = runP (getIteration Done)                     putIterations (cpToken cp) pid 
runCmd cp "current"   [pid]          = runP (getIteration Current)                  putIterations (cpToken cp) pid
runCmd cp "backlog"   [pid]          = runP (getIteration Backlog)                  putIterations (cpToken cp) pid
runCmd cp "iterations"[pid]          = runP getIterations                           putIterations (cpToken cp) pid
runCmd cp "iterations"[pid,l,o]      = runP (getPagedIterations (read l) (read o))  putIterations (cpToken cp) pid 
runCmd _  _           _              = printUsage

runP :: TrackerM a -> (a -> IO ()) -> String -> String -> IO ()
runP cmd io tk pid = runTrackerM cmd tk pid >>= io

printUsage :: IO ()
printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects\n\
                      \trackerh project PROJECT_ID\n\
                      \trackerh stories PROJECT_ID\n\
                      \trackerh stories PROJECT_ID LIMIT OFFSET\n\
                      \trackerh search PROJECT_ID QUERY\n\
                      \trackerh mywork PROJECT_ID USER\n\
                      \trackerh story PROJECT_ID STORY_ID\n\
                      \trackerh delete PROJECT_ID STORY_ID\n\
                      \trackerh add PROJECT_ID TITLE\n\
                      \trackerh comment PROJECT_ID STORY_ID COMMENT\n\
                      \trackerh deliver PROJECT_ID\n\
                      \trackerh done PROJECT_ID\n\
                      \trackerh current PROJECT_ID\n\
                      \trackerh backlog PROJECT_ID\n\
                      \trackerh iterations PROJECT_ID\n\
                      \trackerh iterations PROJECT_ID LIMIT OFFSET\n\
                      \\n"

loadCP :: Maybe FilePath -> IO ConfigParser
loadCP Nothing   = getUserDocumentsDirectory >>= \userDir ->
                   loadCP $ Just $ userDir ++ "/.trackerh"
loadCP (Just fp) = loadConfig fp

loadConfig :: FilePath -> IO ConfigParser
loadConfig fp = forceEither <$> readfile emptyCP fp

forceGet :: String -> ConfigParser -> String
forceGet k cp = forceEither $ get cp "" k

cpToken :: ConfigParser -> String
cpToken = forceGet "token"

putItems :: (a -> IO ()) -> [a] -> IO ()
putItems putFunction i = mapM_ (\s -> putStrLn "" >> putFunction s) i

putItem :: [(a -> String, String)] -> a -> IO ()
putItem attrMap i = mapM_ (\(attr, l) -> putStrLn $ l ++ ": " ++ attr i) attrMap

putProjects :: [Project] -> IO ()
putProjects = putItems putProject

putProject :: Project -> IO ()
putProject = putItem [ (prjName, "Name")
                     , (prjID, "ID")
                     , (prjIterationLength, "Iteration Length")
                     , (prjWeekStartDay, "Start Day")
                     , (prjPointScale, "Point Scale")
                     ]

putIterations :: [Iteration] -> IO ()
putIterations = mapM_ putIteration

putIteration :: Iteration -> IO ()
putIteration itr = putItem [ (show . itrID, "ID")
                           , (itrNumber, "Number") 
                           , (itrStartDate, "Start")
                           , (itrEndDate, "End") 
                           ]  itr >>
               putStories (itrStories itr)


putStories :: [Story] -> IO ()
putStories = putItems putStory

putStory :: Story -> IO ()
putStory = putItem [ (show . stName          ,"Name")
                   , (show . stID            ,"ID")
                   , (show . stType          ,"Type")
                   , (show . stURL           ,"URL")
                   , (show . stEstimate      ,"Estimate")
                   , (show . stCurrentState  ,"Status")
                   , (show . stRequestedBy   ,"Requestor")
                   , (show . stCreatedAt     ,"Created")
                   , (show . stLabels        ,"Labels")
                   , (show . stDescription   ,"Description")
                   ]


putNote :: Note -> IO ()
putNote = putItem [ (show . ntID     , "ID")
                  , (show . ntText   , "Text")
                  , (show . ntAuthor , "Author")
                  , (show . ntNotedAt, "Noted At")
                  ]

putActivities :: [Activity] -> IO ()
putActivities = putItems putActivity

putActivity :: Activity -> IO ()
putActivity = putItem [ (actID          , "ID") 
                      , (actProject     , "Project") 
                      , (actStory       , "Story") 
                      , (actDescription , "Description") 
                      , (actAuthor      , "Author") 
                      , (actWhen        , "When") 
                      ]