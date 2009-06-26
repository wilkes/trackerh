module Tracker.Api
    ( module Tracker.Types
    , module Tracker.Search
    , runProjectM
    , ProjectM
    , getToken
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

getToken :: String -> String -> IO String
getToken username password = callRemote url opts >>=
                          runUnpickle xpToken >>=
                          return . tkGuid . head
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

getProjects :: ProjectM Projects
getProjects = projectURL >>= unpickleWith xpProjects

getProject :: ProjectM Project
getProject = projectURL >>= unpickle

deliverAllFinished :: ProjectM Stories
deliverAllFinished = url >>= tokenPUT [] >>= unpickleResponse xpStories
    where url = storiesURL <++> "/deliver_all_finished"

getStories :: Int -> Int -> ProjectM Stories
getStories limit offset = url >>= unpickleWith xpStories
    where url = storiesURL <++> (limitAndOffset limit offset)

getStory :: String -> ProjectM Story
getStory sid = url >>= unpickle 
    where url = storiesURL <++> ("/" ++ sid)

filterStories :: SearchTerm -> ProjectM [Story]
filterStories = search . show

search :: String -> ProjectM [Story]
search qstring = url >>= unpickleWith xpStories
    where url = storiesURL <++> ("?filter=" ++ escapedQuery)
          escapedQuery = escapeURIString isUnescapedInURI qstring

addStory :: String -> ProjectM Story
addStory title = createStory $ emptyStory {stName = Just title}

createStory :: Story -> ProjectM Story
createStory st = storiesURL >>= pushEntity st xpStory tokenPOST

updateStory :: Story -> ProjectM Story
updateStory st = url >>= pushEntity st xpStory tokenPUT
      where url = storiesURL <++> ("/" ++ stid)
            stid = case (stID st) of
                     Nothing -> ""
                     Just x  -> x

deleteStory :: String -> ProjectM Story
deleteStory sid = storiesURL <++> ("/" ++ sid) >>= tokenDELETE >>= unpickleResponse xpickle

addComment :: String -> String -> ProjectM Note
addComment sid txt = url >>= pushEntity n xpNote tokenPOST
    where url = storiesURL <++> ("/" ++ sid ++ "/notes")
          n = emptyNote {ntText = txt}

getIterations :: String -> ProjectM [Iteration]
getIterations gname = url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations/" ++ gname) 

getPagedIterations :: Int -> Int -> ProjectM [Iteration]
getPagedIterations limit offset = url >>= unpickleWith xpIterations
    where url = projectURL <++> ("/iterations" ++ (limitAndOffset limit offset))

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o

