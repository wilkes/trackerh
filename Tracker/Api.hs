module Tracker.Api where

import Tracker.Types
import qualified Network.Curl as Curl
import Network.URI
import Text.XML.HaXml

serviceURL = "https://www.pivotaltracker.com/services/v2/"
projectURL = serviceURL ++ "projects/" 

token :: String -> String -> IO ()
token username password = callRemote url opts (putStrLn . getToken . parseResponse)
    where url = "https://www.pivotaltracker.com/services/tokens/active"
          opts = [Curl.CurlUserPwd $ username ++ ":" ++ password]

getToken :: Content -> String
getToken e = verbatim $ tag "token" /> tag "guid" /> txt $ e

projects :: String -> IO ()
projects token = tokenCall url token (putRecord xml2Projects)
    where url = serviceURL ++ "projects"

project :: String -> String -> IO ()
project token projectID = tokenCall url token (putRecord xml2Project)
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
    do (code, res) <- Curl.curlGetString url opts
       case code of
         Curl.CurlOK -> callback res
         _ -> fail $ show code

putRecord :: (Show a) => (Content -> a) -> String -> IO ()
putRecord transformer = putStrLn . show . transformer . parseResponse

putStories :: [Story] -> IO ()
putStories = mapM_ (\s -> putStrLn "" >> putStory s)

putStory :: Story -> IO ()
putStory story = 
    do mapM_ display attrMap
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


parseResponse :: String -> Content
parseResponse = content . (xmlParse "response")
    where content (Document _ _ e _) = CElem e

xml2Stories :: Content -> [Story]
xml2Stories e = map xml2Story (tag "stories" /> tag "story" $ e)

xml2Story :: Content -> Story
xml2Story e = 
    Story { stID           = st "id",
            stType         = st "story_type",
            stURL          = st "url",
            stEstimate     = st "estimate",
            stCurrentState = st "current_state",
            stDescription  = st "description",
            stName         = st "name",
            stRequestedBy  = st "requested_by",
            stCreatedAt    = st "craeted_at",
            stLabels       = st "labels" }
    where st k = verbatim $ tag "story" /> tag k /> txt $ e

xml2Projects :: Content -> [Project]
xml2Projects e = map xml2Project (tag "projects" /> tag "project" $ e)

xml2Project :: Content -> Project
xml2Project e = 
    Project { prjID = st "id",
              prjName = st "name",
              prjIterationLength = st "iteration_length",
              prjWeekStartDay = st "week_start_day",
              prjPointScale = st "point_scale" }
    where st k = verbatim $ tag "project" /> tag k /> txt $ e