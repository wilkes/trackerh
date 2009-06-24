module Tracker.Api
    ( module Tracker.Types
    , token
    , project
    , projects
    , story
    , stories
    , search
    , addStory
    , deleteStory
    , updateStory
    , addComment
    , deliverAllFinished
    , iterations
    , paginatedIterations
    )
    where

import Text.XML.HXT.Arrow.Pickle

import Network.Curl
import Network.URI

import Tracker.Types
import Tracker.Pickle

type TokenSt   = String
type ProjectID = String
type StoryID   = String

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: String
projectURL = serviceURL ++ "projects"

storiesURL :: ProjectID -> String
storiesURL pid = projectURL ++ "/" ++ pid ++ "/stories"

token :: String -> String -> IO TokenSt
token username password = callRemote url opts >>=
                          runUnpickle xpToken >>=
                          return . tkGuid . head
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]


projects :: TokenSt -> IO Projects
projects t = unpickleWith t projectURL xpProjects

project :: TokenSt -> ProjectID -> IO Project
project t pid = unpickle t $ projectURL ++ "/" ++ pid

deliverAllFinished :: TokenSt -> ProjectID -> IO Stories
deliverAllFinished t pid = tokenPUT t url [] >>= runUnpickle xpStories >>= return . head
    where url = (storiesURL pid) ++ "/deliver_all_finished"


stories :: TokenSt -> ProjectID -> Int -> Int -> IO Stories
stories t pid limit offset = unpickleWith t url xpStories
    where url = (storiesURL pid) ++ (limitAndOffset limit offset)

story :: TokenSt -> ProjectID -> StoryID -> IO Story
story t pid sid = unpickle t url 
    where url = (storiesURL pid) ++ "/" ++ sid

search :: TokenSt -> ProjectID -> String -> IO [Story]
search t pid qstring = unpickleWith t url xpStories
    where url = (storiesURL pid) ++ "?filter=" ++ escapedQuery
          escapedQuery = escapeURIString isUnescapedInURI qstring

addStory :: TokenSt -> ProjectID -> String -> IO Story
addStory t pid title = createStory t pid $ emptyStory {stName = Just title}

createStory :: TokenSt -> ProjectID -> Story -> IO Story
createStory t pid st = pushEntity st xpStory (tokenPOST t (storiesURL pid))

updateStory :: TokenSt -> ProjectID -> Story -> IO Story
updateStory t pid st = pushEntity st xpStory (tokenPUT t url)
    where url = (storiesURL pid) ++ "/" ++ stid
          stid = case (stID st) of
                   Nothing -> ""
                   Just x  -> x

deleteStory :: TokenSt -> ProjectID -> StoryID -> IO Story
deleteStory t pid sid = tokenDELETE t url >>=
                                  runUnpickle xpickle >>=
                                  return . head
    where url = (storiesURL pid) ++ "/" ++ sid


addComment :: TokenSt -> ProjectID -> StoryID -> String -> IO Note
addComment t pid sid txt = pushEntity n xpNote (tokenPOST t url)
    where n = emptyNote {ntText = txt}
          url = (storiesURL pid) ++ "/" ++ sid ++ "/notes"

iterations :: TokenSt -> ProjectID -> String -> IO [Iteration]
iterations t pid gname = unpickleWith t url xpIterations
    where url = projectURL ++ "/" ++ pid ++ "/iterations/" ++ gname

paginatedIterations :: TokenSt -> ProjectID -> Int -> Int -> IO [Iteration]
paginatedIterations t pid limit offset = unpickleWith t url xpIterations
    where url = projectURL ++ "/" ++ pid ++ "/iterations" ++
                (limitAndOffset limit offset)

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o

pushEntity :: (XmlPickler a) => a -> PU a -> ([String] -> IO String) -> IO a
pushEntity entity pickler webAction = runPickle pickler entity >>=
                                      webAction >>=
                                      runUnpickle pickler >>=
                                      return . head

unpickle :: (XmlPickler a) => String -> String -> IO a
unpickle t url = unpickleWith t url xpickle

unpickleWith :: (XmlPickler a) => String -> String -> PU a -> IO a
unpickleWith t url xp = tokenGET t url >>= runUnpickle xp >>= return . head 

tokenPOST :: TokenSt -> String -> [String] -> IO String
tokenPOST t url ps = callRemote url opts
    where opts = [ defaultHeaders t
                 , CurlPostFields ps
                 , CurlPost True
                 ]

tokenPUT :: TokenSt -> String -> [String] -> IO String
tokenPUT t url ps = callRemote url [ defaultHeaders t
                                   , CurlCustomRequest "PUT"
                                   , CurlPostFields ps
                                   ]

tokenDELETE :: TokenSt -> String -> IO String
tokenDELETE t url = callRemote url [ defaultHeaders t
                                   , CurlPost False
                                   , CurlCustomRequest "DELETE"
                                   ]

tokenGET :: TokenSt -> String -> IO String
tokenGET t url = callRemote url [defaultHeaders t]

callRemote :: String -> [CurlOption] -> IO String
callRemote url opts = 
    getResponse >>= \response ->
        case (respCurlCode response) of
          CurlOK -> return $ respBody response
          _      -> fail $ msg response 
    where getResponse :: IO (CurlResponse_ [(String, String)] String)
          getResponse = curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r

defaultHeaders :: TokenSt -> CurlOption
defaultHeaders t = CurlHttpHeaders [ "X-TrackerToken: " ++ t
                                   , "Content-type: application/xml"
                                   ]

