module Tracker.Api where

import Network.Curl
import Network.URI

import Tracker.Types
import Tracker.Xml

serviceURL = "https://www.pivotaltracker.com/services/v2/"
projectURL = serviceURL ++ "projects"
storiesURL pid = projectURL ++ "/" ++ pid ++ "/stories"

token :: String -> String -> IO ()
token username password = callRemote url opts (putStrLn . getToken)
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [CurlUserPwd $ username ++ ":" ++ password]

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
    where opts = [CurlHttpHeaders ["X-TrackerToken: " ++ token,
                                   "Content-type: application/xml"]]

callRemote :: String -> [CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    getResponse >>= \response ->
        case (respCurlCode response) of
          CurlOK -> callback $ respBody response
          _ -> fail $ msg response 
    where getResponse :: IO (CurlResponse_ [(String, String)] String)
          getResponse = curlGetResponse_ url opts
          msg r = url ++ "\n" ++ 
                  (show $ respStatus r) ++ respStatusLine r

putItems :: (a -> IO ()) -> [a] -> IO ()
putItems putFunction items = mapM_ (\s -> putStrLn "" >> putFunction s) items

putItem :: [(a -> String, String)] -> a -> IO ()
putItem attrMap item = mapM_ (\(attr, l) -> putStrLn $ l ++ ": " ++ (attr item)) attrMap

putProjects :: [Project] -> IO ()
putProjects = putItems putProject

putProject :: Project -> IO ()
putProject = putItem [(prjName, "Name"),
                      (prjID, "ID"),
                      (prjIterationLength, "Iteration Length"),
                      (prjWeekStartDay, "Start Day"),
                      (prjPointScale, "Point Scale")]

putStories :: [Story] -> IO ()
putStories = putItems putStory

putStory :: Story -> IO ()
putStory = putItem [(stName         ,"Name"),
                    (stID           ,"ID"),
                    (stType         ,"Type"),
                    (stURL          ,"URL"),
                    (stEstimate     ,"Estimate"),
                    (stCurrentState ,"Status"),
                    (stRequestedBy  ,"Requestor"),
                    (stCreatedAt    ,"Created"),
                    (stLabels       ,"Labels"),
                    (stDescription  ,"Description")]
      

