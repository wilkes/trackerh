module Tracker.Api
    ( module Tracker.Types
    , StoryID
    , getToken
    , getActivities
    , getProjects
    , getProject
    , getStories
    , getStory
    , search
    , filterStories
    , addStory
    , updateStory
    , deleteStory
    , addComment
    , deliverAllFinished
    , getIteration
    , getIterations
    , getPagedIterations
    , mapAll
    , mapProjects
    )
    where

import Control.Applicative((<$>))
import Control.Concurrent
import Control.Monad(replicateM,zipWithM_)
import Data.Char(toLower)
import Network.URI(escapeURIString,isUnescapedInURI)
import Network.Curl(CurlOption(..))
import Text.XML.HXT.Arrow.Pickle(xpickle)

import Tracker.Context
import Tracker.Types
import Tracker.Pickle

type StoryID = String

-- | Given a user name and password fetch the corresponding API Token
getToken :: String -> String -> IO String
getToken username password =
    (tkGuid . head) <$> (callRemote url opts >>= runUnpickle xpToken)
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

-- | Get all the projects associated with the current API token
getProjects :: TrackerM Projects
getProjects = projectURL >>= unpickleWith xpProjects

-- | Get a single project
getProject :: TrackerM Project
getProject = projectURL >>= unpickle

-- | Mark all finished stories as delivered
deliverAllFinished :: TrackerM Stories
deliverAllFinished = url >>= doPut [] >>= unpickleResponse xpStories
    where url = storiesURL <++> "/deliver_all_finished"

-- | Get all the stories for a project
getStories :: Int -> Int -> TrackerM Stories
getStories limit offset = url >>= unpickleWith xpStories
    where url = storiesURL <++> limitAndOffset limit offset

-- | Get a specific story
getStory :: StoryID -> TrackerM Story
getStory sid = url >>= unpickle 
    where url = storiesURL <++> ("/" ++ sid)

-- | Search for a list of stories using the given criteria
filterStories :: SearchTerm -> TrackerM [Story]
filterStories = search . show

-- | Search for a list of stories using the given filter string
search :: String -> TrackerM [Story]
search qstring = url >>= unpickleWith xpStories
    where url = storiesURL <++> ("?filter=" ++ escapedQuery)
          escapedQuery = escapeURIString isUnescapedInURI qstring

-- | Add a story with a given title
addStory :: String -> TrackerM Story
addStory title = createStory $ emptyStory {stName = Just title}

createStory :: Story -> TrackerM Story
createStory st = storiesURL >>= pushEntity st xpStory doPost

updateStory :: Story -> TrackerM Story
updateStory st = url >>= pushEntity st xpStory doPut
      where url = storiesURL <++> ("/" ++ stid)
            stid = case (stID st) of
                     Nothing -> ""
                     Just x  -> x

deleteStory :: StoryID -> TrackerM Story
deleteStory sid = url >>= doDelete >>= unpickleResponse xpickle
    where url = storiesURL <++> ("/" ++ sid)

-- | Append a comment to the story with the given story id.
addComment :: StoryID -> String -> TrackerM Note
addComment sid txt = url >>= pushEntity n xpNote doPost
    where url = storiesURL <++> ("/" ++ sid ++ "/notes")
          n = emptyNote {ntText = txt}

-- | Get Stories grouped by iteration with a given name 
getIteration :: NamedIteration -> TrackerM [Iteration]
getIteration n = case n of
                   Icebox -> getIcebox
                   _      -> url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations/" ++ (map toLower $ show n))

getIcebox :: TrackerM Iterations
getIcebox = do
  stories <- filterStories (State Unscheduled)
  return $ [emptyIteration {itrStories = stories}]

-- | Get Stories grouped by iteration with a given name 
getIterations :: TrackerM [Iteration]
getIterations = url >>= unpickleWith xpIterations
    where url = projectURL <++> "/iterations" 

-- | Get stories grouped by iteration and paged with limit and offset
getPagedIterations :: Int -> Int -> TrackerM [Iteration]
getPagedIterations limit offset = url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations" ++ limitAndOffset limit offset)

-- | Get recent activity for all projects or the project in the TrackerM
getActivities :: TrackerM [Activity]
getActivities = activitiesURL >>= unpickleWith xpActivities

-- | Collect the results of an API call across all the projects
-- associated with the given token
mapAll :: String -> TrackerM a -> IO [(Project, a)]
mapAll tk f = do
  projects <- runTrackerM getProjects tk ""
  rs <- mapProjects tk f (map prjID projects)
  return $ zip projects rs

-- | Collect the results of an API call across the list of given
-- project ids.
mapProjects :: String -> TrackerM a -> [String] -> IO [a]
mapProjects tk f pids = orderedForkIO $ map (runTrackerM f tk) pids

orderedForkIO :: [IO a] -> IO [a]
orderedForkIO actions = do
  mvars <- replicateM (length actions) newEmptyMVar
  let run mvar action = forkIO $ action >>= putMVar mvar
  forkIO $ zipWithM_ run mvars actions
  mapM takeMVar mvars

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o

