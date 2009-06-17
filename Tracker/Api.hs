module Tracker.Api
    ( module Tracker.Types
    , token
    , project
    , projects
    , story
    , stories
    , search
    , addStory
    , iterationGroup
    )
    where
import Control.Applicative((<$>))
import Network.Curl
import Network.URI

import Tracker.Types
import Tracker.Xml

type Token     = String
type ProjectID = String
type StoryID   = String

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: String
projectURL = serviceURL ++ "projects"

storiesURL :: String -> String
storiesURL pid = projectURL ++ "/" ++ pid ++ "/stories"

token :: String -> String -> IO Token
token username password = getToken <$> callRemote url opts
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

projects :: Token -> IO [Project]
projects t = toRecords <$> tokenCall t projectURL

project :: Token -> ProjectID -> IO Project
project t projectID = toRecord <$> tokenCall t url
    where url = projectURL ++ "/" ++ projectID

stories :: Token -> ProjectID -> IO [Story]
stories t projectID = toRecords <$> tokenCall t (storiesURL projectID)

story :: Token -> ProjectID -> StoryID -> IO Story
story t projectID storyID = toRecord <$> tokenCall t url 
    where url = (storiesURL projectID) ++ "/" ++ storyID

search :: Token -> ProjectID -> String -> IO [Story]
search t projectID qstring = toRecords <$> tokenCall t url
    where url = (storiesURL projectID) ++ "?filter=" ++ escapedQuery
          escapedQuery = escapeURIString isUnescapedInURI qstring

addStory :: Token -> ProjectID -> String -> IO Story
addStory t projectID title = toRecord <$> tokenPost t (storiesURL projectID) postData
    where postData = ["<story><name>" ++ title ++ "</name></story>"]

iterationGroup :: Token -> ProjectID -> String -> IO [Iteration]
iterationGroup t projectID gname = toRecords <$> tokenCall t url
    where url = projectURL ++ "/" ++ projectID ++ "/iterations/" ++ gname

tokenPost :: Token -> String -> [String] -> IO String
tokenPost t url ps = callRemote url opts
    where opts = [ CurlHttpHeaders ["X-TrackerToken: " ++ t,
                                    "Content-type: application/xml"]
                 , CurlPostFields ps
                 , CurlPost True]

tokenCall :: Token -> String -> IO String
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
          getResponse = curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r

