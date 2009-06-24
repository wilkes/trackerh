module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Directory
import Data.ConfigFile
import Data.Either.Utils(forceEither)

import Tracker.Api

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
runCmd _  "token"     [uid, pwd]         = putStrLn      =<< token uid pwd
runCmd cp "project"   [pid]              = putProject    =<< project  (getToken cp) pid
runCmd cp "projects"  _                  = putProjects   =<< projects (getToken cp)
runCmd cp "story"     [pid, sid]         = putStory      =<< story    (getToken cp) pid sid
runCmd cp "delete"    [pid, sid]         = putStory      =<< deleteStory (getToken cp) pid sid
runCmd cp "stories"   [pid]              = putStories    =<< stories  (getToken cp) pid 0 0
runCmd cp "stories"   [pid,limit,offset] = putStories    =<< stories  (getToken cp) pid (read limit) (read offset)
runCmd cp "search"    (pid:rest)         = putStories    =<< search   (getToken cp) pid (intercalate " " rest)
runCmd cp "add"       (pid:rest)         = putStory      =<< addStory (getToken cp) pid (intercalate " " rest)
runCmd cp "comment"   (pid:sid:rest)     = putNote       =<< addComment (getToken cp) pid sid (intercalate " " rest)
runCmd cp "deliver"   [pid]              = putStories    =<< deliverAllFinished (getToken cp) pid
runCmd cp "done"      [pid]              = putIterations =<< iterations (getToken cp) pid "done"
runCmd cp "current"   [pid]              = putIterations =<< iterations (getToken cp) pid "current"
runCmd cp "backlog"   [pid]              = putIterations =<< iterations (getToken cp) pid "backlog"
runCmd cp "iterations"[pid]              = putIterations =<< iterations (getToken cp) pid ""
runCmd cp "iterations"[pid,limit,offset] = putIterations =<< paginatedIterations (getToken cp) pid (read limit) (read offset)
runCmd _  _           _                  = printUsage

loadCP :: Maybe FilePath -> IO ConfigParser
loadCP Nothing   = getUserDocumentsDirectory >>= \userDir ->
                   loadCP $ Just $ userDir ++ "/.trackerh"
loadCP (Just fp) = loadConfig fp

loadConfig :: FilePath -> IO ConfigParser
loadConfig fp = readfile emptyCP fp >>= return . forceEither

forceGet :: String -> ConfigParser -> String
forceGet k cp = forceEither $ get cp "" k

getToken :: ConfigParser -> String
getToken    = forceGet "token"

printUsage :: IO ()
printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects\n\
                      \trackerh project PROJECT_ID\n\
                      \trackerh stories PROJECT_ID\n\
                      \trackerh stories PROJECT_ID LIMIT OFFSET\n\
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

putItems :: (a -> IO ()) -> [a] -> IO ()
putItems putFunction i = mapM_ (\s -> putStrLn "" >> putFunction s) i

putItem :: [(a -> String, String)] -> a -> IO ()
putItem attrMap i = mapM_ (\(attr, l) -> putStrLn $ l ++ ": " ++ (attr i)) attrMap

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