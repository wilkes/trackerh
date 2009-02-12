module Tracker.Api where

import qualified Network.Curl as Curl
import Network.URI

import Tracker.Types
import Tracker.Xml

serviceURL = "https://www.pivotaltracker.com/services/v2/"
projectURL = serviceURL ++ "projects"
storiesURL pid = projectURL ++ "/" ++ pid ++ "/stories"

token :: String -> String -> IO ()
token username password = callRemote url opts (putStrLn . getToken)
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]

projects :: String -> IO ()
projects token = tokenCall token (putProjects . toRecords) projectURL

project :: String -> String -> IO ()
project token projectID = tokenCall token (putProject . toRecord) url
    where url = projectURL ++ "/" ++ projectID

stories :: String -> String -> IO ()
stories token projectID = tokenCall token (putStories . toRecords) $ storiesURL projectID

story :: String -> String -> String -> IO ()
story token projectID storyID = tokenCall token (putStory . toRecord) url 
    where url = (storiesURL projectID) ++ "/" ++ storyID

search :: String -> String -> String -> IO ()
search token projectID filter = tokenCall token (putStories . toRecords) url
    where url = (storiesURL projectID) ++ "?filter=" ++ query
          query = escapeURIString isUnescapedInURI filter

tokenCall :: String -> (String -> IO ()) -> String -> IO ()
tokenCall token callback url = callRemote url opts callback
    where opts = [Curl.CurlHttpHeaders ["X-TrackerToken: " ++ token,
                                        "Content-type: application/xml"]]

callRemote :: String -> [Curl.CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    getResponse >>= \response ->
        case (Curl.respCurlCode response) of
          Curl.CurlOK -> callback $ Curl.respBody response
          _ -> fail $ msg response 
    where getResponse :: IO (Curl.CurlResponse_ [(String, String)] String)
          getResponse = Curl.curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ Curl.respStatus r) ++ Curl.respStatusLine r

putProjects :: [Project] -> IO ()
putProjects = mapM_ (\s -> putStrLn "" >> putProject s)

putProject :: Project -> IO ()
putProject project = mapM_ display attrMap
      where display (attr, label) = putStrLn $ label ++ ": " ++ (attr project)
            attrMap =[ (prjName, "Name"),
                       (prjID, "ID"),
                       (prjIterationLength, "Iteration Length"),
                       (prjWeekStartDay, "Start Day"),
                       (prjPointScale, "Point Scale")]

putStories :: [Story] -> IO ()
putStories = mapM_ (\s -> putStrLn "" >> putStory s)

putStory :: Story -> IO ()
putStory story = mapM_ display attrMap
      where display (attr, label) = putStrLn $ label ++ ": " ++ (attr story)
            attrMap = [(stName         ,"Name"),
                       (stID           ,"ID"),
                       (stType         ,"Type"),
                       (stURL          ,"URL"),
                       (stEstimate     ,"Estimate"),
                       (stCurrentState ,"Status"),
                       (stRequestedBy  ,"Requestor"),
                       (stCreatedAt    ,"Created"),
                       (stLabels       ,"Labels"),
                       (stDescription  ,"Description")]

