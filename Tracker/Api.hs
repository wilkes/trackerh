module Tracker.Api where

import qualified Network.Curl as Curl
import Network.URI

import Tracker.Types
import Tracker.Xml

serviceURL = "https://www.pivotaltracker.com/services/v2/"
projectURL = serviceURL ++ "projects/" 

token :: String -> String -> IO ()
token username password = callRemote url opts (putStrLn . getToken . parseResponse)
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]

projects :: String -> IO ()
projects token = tokenCall url token (putProjects . xml2Projects . parseResponse)
    where url = serviceURL ++ "projects"

project :: String -> String -> IO ()
project token projectID = tokenCall url token (putProject . xml2Project . parseResponse)
    where url = projectURL ++ projectID

stories :: String -> String -> IO ()
stories token projectID = tokenCall url token (putStories . xml2Stories . parseResponse)
    where url = projectURL ++ projectID ++ "/stories"

story :: String -> String -> String -> IO ()
story token projectID storyID = tokenCall url token (putStory . xml2Story . parseResponse)
    where url = projectURL ++ projectID ++ "/stories/" ++ storyID

search :: String -> String -> String -> IO ()
search token projectID filter = 
    tokenCall url token (putStories . xml2Stories . parseResponse)
    where url = projectURL ++ projectID ++ "/stories?filter=" ++ query
          query = escapeURIString isUnescapedInURI filter

tokenCall :: String -> String -> (String -> IO ()) -> IO ()
tokenCall url token callback = callRemote url opts callback
    where opts = [Curl.CurlHttpHeaders ["X-TrackerToken: " ++ token, "Content-type: application/xml"]]

callRemote :: String -> [Curl.CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    Curl.curlGetString url opts >>= \(code, res) ->
        case code of
          Curl.CurlOK -> callback res
          _ -> fail $ show code

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

