module Main where

import System.Environment
import qualified Network.Curl as Curl
import Text.XML.HaXml
import TrackerTypes

main = do args <- getArgs
          case args of
            ["token", username, password]        -> token username password
            ["projects", token]                  -> projects token
            ["project", token, projectID]        -> project token projectID
            ["stories", token, projectID]        -> stories token projectID
            ["story", token, projectID, storyID] -> story token projectID storyID
            _ -> printUsage

printUsage = putStrLn "Usage: trackerh command [args]\n\
                      \trackerh token username password\n\
                      \trackerh projects token\n\
                      \trackerh project token projectID\n\
                      \trackerh stories token projectID\n\
                      \trackerh story token projectID storyID\n\
                      \\n"

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
stories token projectID = tokenCall url token (putRecord xml2Stories)
    where url = projectURL ++ projectID ++ "/stories"

story :: String -> String -> String -> IO ()
story token projectID storyID = tokenCall url token (putRecord xml2Story)
    where url = projectURL ++ projectID ++ "/stories/" ++ storyID

tokenCall :: String -> String -> (String -> IO ()) -> IO ()
tokenCall url token callback = callRemote url opts callback
    where opts = [Curl.CurlHttpHeaders ["X-TrackerToken: " ++ token, "Content-type: application/xml"]]

callRemote :: String -> [Curl.CurlOption] -> (String -> IO ()) -> IO () 
callRemote url opts callback = 
    do response <- getResponse
       case (Curl.respCurlCode response) of
         Curl.CurlOK -> callback $ Curl.respBody response
         _ -> fail $ Curl.respStatusLine response
    where getResponse :: IO (Curl.CurlResponse_ [(String, String)] String)
          getResponse = Curl.curlGetResponse_ url opts

putRecord :: (Show a) => (Content -> a) -> String -> IO ()
putRecord transformer = putStrLn . show . transformer . parseResponse

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