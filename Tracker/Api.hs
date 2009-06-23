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
    , iterations
    , paginatedIterations
    )
    where

import Text.XML.HXT.Arrow
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
token username password = callRemote url opts >>= runUnpickle xpToken >>= return . tkGuid . head
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

projects :: TokenSt -> IO Projects
projects t = unpickleMany t projectURL xpProjects

project :: TokenSt -> ProjectID -> IO Project
project t projectID = unpickleOne t url 
    where url = projectURL ++ "/" ++ projectID

stories :: TokenSt -> ProjectID -> Int -> Int -> IO Stories
stories t projectID limit offset = unpickleMany t url xpStories
    where url = (storiesURL projectID) ++ (limitAndOffset limit offset)

story :: TokenSt -> ProjectID -> StoryID -> IO Story
story t projectID storyID = unpickleOne t url 
    where url = (storiesURL projectID) ++ "/" ++ storyID

deleteStory :: TokenSt -> ProjectID -> StoryID -> IO Story
deleteStory t projectID storyID = tokenDelete t url >>= runUnpickle xpickle >>= return . head
    where url = (storiesURL projectID) ++ "/" ++ storyID

search :: TokenSt -> ProjectID -> String -> IO [Story]
search t projectID qstring = unpickleMany t url xpStories
    where url = (storiesURL projectID) ++ "?filter=" ++ escapedQuery
          escapedQuery = escapeURIString isUnescapedInURI qstring

addStory :: TokenSt -> ProjectID -> String -> IO Story
addStory t projectID title = tokenPost t (storiesURL projectID) postData >>=
                             runUnpickle xpStory >>=
                             return . head
    where postData = ["<story><name>" ++ title ++ "</name></story>"]

iterations :: TokenSt -> ProjectID -> String -> IO [Iteration]
iterations t projectID gname = unpickleMany t url xpIterations
    where url = projectURL ++ "/" ++ projectID ++ "/iterations/" ++ gname

paginatedIterations :: TokenSt -> ProjectID -> Int -> Int -> IO [Iteration]
paginatedIterations t projectID limit offset = unpickleMany t url xpIterations
    where url = projectURL ++ "/" ++ projectID ++ "/iterations" ++ (limitAndOffset limit offset)

limitAndOffset :: Int -> Int -> String
limitAndOffset l o
    | l <= 0    = ""
    | otherwise = "?limit=" ++ show l ++ offset
    where offset | o <= 0 = ""
                 | otherwise = "&offset=" ++ show o

tokenPost :: TokenSt -> String -> [String] -> IO String
tokenPost t url ps = callRemote url opts
    where opts = [ CurlHttpHeaders ["X-TrackerToken: " ++ t,
                                    "Content-type: application/xml"]
                 , CurlPostFields ps
                 , CurlPost True]

tokenDelete :: TokenSt -> String -> IO String
tokenDelete t url = callRemote url opts
    where opts = [ CurlHttpHeaders ["X-TrackerToken: " ++ t]
                 , CurlPost False
                 , CurlCustomRequest "DELETE"
                 ]

unpickleOne :: (XmlPickler a) => String -> String -> IO a
unpickleOne t url = unpickleMany t url xpickle

unpickleMany :: (XmlPickler a) => String -> String -> PU a -> IO a
unpickleMany t url xp = tokenCall t url >>= runUnpickle xp >>= return . head 

runUnpickle :: (XmlPickler a) => PU a -> String -> IO [a]
runUnpickle xp xml = runX $ readString options xml >>> xunpickleVal xp
    where options = [ (a_validate,v_0)
		    , (a_remove_whitespace,v_1)
                    -- , (a_trace,v_1)
		    , (a_preserve_comment, v_0)
		    ]

tokenCall :: TokenSt -> String -> IO String
tokenCall t url = callRemote url opts
    where opts = [CurlHttpHeaders ["X-TrackerToken: " ++ t,
                                   "Content-type: application/xml"]]

callRemote :: String -> [CurlOption] -> IO String
callRemote url opts = 
    getResponse >>= \response ->
        case (respCurlCode response) of
          CurlOK -> return $ respBody response
          _      -> fail $ msg response 
    where getResponse :: IO (CurlResponse_ [(String, String)] String)
          getResponse = curlGetResponse_ url opts -- >>= \r -> putStrLn (respBody r) >> return r
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r

