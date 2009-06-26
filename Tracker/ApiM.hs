module Tracker.ApiM where

import Network.URI
import Network.Curl
import Text.XML.HXT.Arrow.Pickle
import Tracker.Context
import Tracker.Types
import Tracker.Pickle
import Tracker.Filter

token :: String -> String -> IO String
token username password = callRemote url opts >>=
                          runUnpickle xpToken >>=
                          return . tkGuid . head
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

projects :: ProjectM Projects
projects = unpickleWithM xpProjects projectURL

project :: ProjectM Project
project = unpickleM =<< projectURLM

deliverAllFinished :: ProjectM Stories
deliverAllFinished = url >>= tokenPUTM [] >>= unpickleResponseM xpStories
    where url = storiesURLM <++> "/deliver_all_finished"

stories :: Int -> Int -> ProjectM Stories
stories limit offset = url >>= unpickleWithM xpStories
    where url = storiesURLM <++> (limitAndOffset limit offset)

story :: String -> ProjectM Story
story sid = url >>= unpickleM 
    where url = storiesURLM <++> ("/" ++ sid)

filterStories :: SearchTerm -> ProjectM [Story]
filterStories = search . show

search :: String -> ProjectM [Story]
search qstring = url >>= unpickleWithM xpStories
    where url = storiesURLM <++> ("?filter=" ++ escapedQuery)
          escapedQuery = escapeURIString isUnescapedInURI qstring

addStory :: String -> ProjectM Story
addStory title = createStory $ emptyStory {stName = Just title}

createStory :: Story -> ProjectM Story
createStory st = storiesURLM >>= pushEntityM st xpStory tokenPOSTM

updateStory :: Story -> ProjectM Story
updateStory st = url >>= pushEntityM st xpStory tokenPUTM
      where url = storiesURLM <++> ("/" ++ stid)
            stid = case (stID st) of
                     Nothing -> ""
                     Just x  -> x

deleteStory :: String -> ProjectM Story
deleteStory sid = storiesURLM <++> ("/" ++ sid) >>= tokenDELETEM >>= unpickleResponseM xpickle

addComment :: String -> String -> ProjectM Note
addComment sid txt = url >>= pushEntityM n xpNote tokenPOSTM
    where url = storiesURLM <++> ("/" ++ sid ++ "/notes")
          n = emptyNote {ntText = txt}

iterations :: String -> ProjectM [Iteration]
iterations gname = url >>= unpickleWithM xpIterations
    where url = projectURLM <++> ("/iterations/" ++ gname) 

paginatedIterations :: Int -> Int -> ProjectM [Iteration]
paginatedIterations limit offset = url >>= unpickleWithM xpIterations
    where url = projectURLM <++> ("/iterations" ++ (limitAndOffset limit offset))

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o

