module Tracker.Api where
import Control.Applicative((<$>))
import Network.Curl
import Network.URI

import Tracker.Types
import Tracker.Xml

serviceURL :: String
serviceURL = "https://www.pivotaltracker.com/services/v2/"

projectURL :: String
projectURL = serviceURL ++ "projects"

storiesURL :: String -> String
storiesURL pid = projectURL ++ "/" ++ pid ++ "/stories"

token' :: String -> String -> IO String
token' username password = getToken <$> callRemote url opts
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

projects :: String -> IO [Project]
projects t = toRecords <$> tokenCall t projectURL

project :: String -> String -> IO Project
project t projectID = toRecord <$> tokenCall t url
    where url = projectURL ++ "/" ++ projectID

stories :: String -> String -> IO [Story]
stories t projectID = toRecords <$> tokenCall t (storiesURL projectID)

story :: String -> String -> String -> IO Story
story t projectID storyID = toRecord <$> tokenCall t url 
    where url = (storiesURL projectID) ++ "/" ++ storyID

search :: String -> String -> String -> IO [Story]
search t projectID qstring = toRecords <$> tokenCall t url
    where url = (storiesURL projectID) ++ "?filter=" ++ escapedQuery
          escapedQuery = escapeURIString isUnescapedInURI qstring

tokenCall :: String -> String -> IO String
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

