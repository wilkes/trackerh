module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Directory
import Data.ConfigFile
import Data.Either.Utils(forceEither)

import Tracker.ApiM
import Tracker.Types
import Tracker.Context
import Tracker.Filter

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
runCmd _  "token"     [uid, pwd]     = putStrLn =<< token uid pwd
runCmd cp "projects"  _              = runP projects                                putProjects   (getToken cp) ""
runCmd cp "project"   [pid]          = runP project                                 putProject    (getToken cp) pid
runCmd cp "stories"   [pid]          = runP (stories 0 0)                           putStories    (getToken cp) pid 
runCmd cp "stories"   [pid,l,o]      = runP (stories (read l) (read o))             putStories    (getToken cp) pid 
runCmd cp "search"    (pid:rest)     = runP (search (intercalate " " rest))         putStories    (getToken cp) pid 
runCmd cp "mywork"    [pid,user]     = runP (filterStories (MyWork user))           putStories    (getToken cp) pid
runCmd cp "story"     [pid, sid]     = runP (story sid)                             putStory      (getToken cp) pid 
runCmd cp "delete"    [pid, sid]     = runP (deleteStory sid)                       putStory      (getToken cp) pid 
runCmd cp "add"       (pid:rest)     = runP (addStory (intercalate " " rest))       putStory      (getToken cp) pid 
runCmd cp "comment"   (pid:sid:rest) = runP (addComment sid (intercalate " " rest)) putNote       (getToken cp) pid
runCmd cp "deliver"   [pid]          = runP deliverAllFinished                      putStories    (getToken cp) pid
runCmd cp "done"      [pid]          = runP (iterations  "done")                    putIterations (getToken cp) pid 
runCmd cp "current"   [pid]          = runP (iterations  "current")                 putIterations (getToken cp) pid
runCmd cp "backlog"   [pid]          = runP (iterations  "backlog")                 putIterations (getToken cp) pid
runCmd cp "iterations"[pid]          = runP (iterations  "")                        putIterations (getToken cp) pid
runCmd cp "iterations"[pid,l,o]      = runP (paginatedIterations (read l) (read o)) putIterations (getToken cp) pid 
runCmd _  _           _              = printUsage

runP :: ProjectM a -> (a -> IO ()) -> String -> String -> IO ()
runP cmd io tk pid = io =<< runProjectM cmd tk pid

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
loadConfig fp = readfile emptyCP fp >>= return . forceEither

forceGet :: String -> ConfigParser -> String
forceGet k cp = forceEither $ get cp "" k

getToken :: ConfigParser -> String
getToken    = forceGet "token"

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