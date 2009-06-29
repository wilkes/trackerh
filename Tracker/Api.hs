module Tracker.Api
    ( module Tracker.Types
    , module Tracker.Search
    , TrackerM
    , runTrackerM
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
    , getIterations
    , getPagedIterations
    )
    where

import Network.URI
import Network.Curl
import Text.XML.HXT.Arrow.Pickle

import Tracker.Context
import Tracker.Types
import Tracker.Pickle
import Tracker.Search

type StoryID = String

-- | Given a user name and password fetch the corresponding API Token
getToken :: String -> String -> IO String
getToken username password = callRemote url opts >>=
                          runUnpickle xpToken >>=
                          return . tkGuid . head
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
deliverAllFinished = url >>= tokenPUT [] >>= unpickleResponse xpStories
    where url = storiesURL <++> "/deliver_all_finished"

-- | Get all the stories for a project
getStories :: Int -> Int -> TrackerM Stories
getStories limit offset = url >>= unpickleWith xpStories
    where url = storiesURL <++> (limitAndOffset limit offset)

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
createStory st = storiesURL >>= pushEntity st xpStory tokenPOST

updateStory :: Story -> TrackerM Story
updateStory st = url >>= pushEntity st xpStory tokenPUT
      where url = storiesURL <++> ("/" ++ stid)
            stid = case (stID st) of
                     Nothing -> ""
                     Just x  -> x

deleteStory :: StoryID -> TrackerM Story
deleteStory sid = storiesURL <++> ("/" ++ sid) >>= tokenDELETE >>= unpickleResponse xpickle

-- | Append a comment to the story with the given story id.
addComment :: StoryID -> String -> TrackerM Note
addComment sid txt = url >>= pushEntity n xpNote tokenPOST
    where url = storiesURL <++> ("/" ++ sid ++ "/notes")
          n = emptyNote {ntText = txt}

-- | Get Stories grouped by iteration with a given name 
getIterations :: String -> TrackerM [Iteration]
getIterations gname = url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations/" ++ gname) 

-- | Get stories grouped by iteration and paged with limit and offset
getPagedIterations :: Int -> Int -> TrackerM [Iteration]
getPagedIterations limit offset = url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations" ++ (limitAndOffset limit offset))

-- | Get recent activity for all projects or the project in the TrackerM
getActivities :: TrackerM [Activity]
getActivities = activitiesURL >>= unpickleWith xpActivities

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o


